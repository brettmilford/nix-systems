{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.gateway;
in
{
  options.services.gateway = {
    enable = mkEnableOption "Enable general reverse proxy config";
  };

  config = mkIf cfg.enable {
    services.nginx = {
      enable = true;

      #recommendedProxySettings = true; # TODO: this breaks some web apps
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;

      commonHttpConfig = let
      realIpsFromList = lib.strings.concatMapStringsSep "\n" (x: "set_real_ip_from  ${x};");
      fileToList = x: lib.strings.splitString "\n" (builtins.readFile x);
      cfipv4 = fileToList (pkgs.fetchurl {
        url = "https://www.cloudflare.com/ips-v4";
        sha256 = "0ywy9sg7spafi3gm9q5wb59lbiq0swvf0q3iazl0maq1pj1nsb7h";
      });
      cfipv6 = fileToList (pkgs.fetchurl {
        url = "https://www.cloudflare.com/ips-v6";
        sha256 = "1ad09hijignj6zlqvdjxv7rjj8567z357zfavv201b9vx3ikk7cy";
      });
      in ''
        ${realIpsFromList cfipv4}
        ${realIpsFromList cfipv6}
        real_ip_header CF-Connecting-IP;
        limit_req_zone $binary_remote_addr zone=auth:10m rate=30r/m;
        limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
        limit_req_zone $binary_remote_addr zone=general:10m rate=30r/m;
        limit_conn_zone $binary_remote_addr zone=general_conn:10m;
      '';
    };

    age.secrets."cf-api-key".file = "${self}/secrets/cf-api-key.age";

    services.fail2ban = let
      cf-email = "brettmilford@gmail.com";
      cf-api-key = config.age.secrets."cf-api-key".path;
    in {
      enable = true;
      extraPackages = [pkgs.curl pkgs.ipset];
      banaction = "iptables-ipset-proto6-allports";
      ignoreIP = [
        "172.22.70.58/16"
      ];

      jails.nginx-noagent = ''
        enabled  = true
        port     = http,https
        filter   = nginx-noagent
        backend  = auto
        maxretry = 1
        logpath  = %(nginx_access_log)s
        action   = cloudflare[cfuser="${cf-email}", cftoken="${cf-api-key}"]
                   iptables-multiport[port="http,https"]
      '';
    };

    environment.etc."fail2ban/filter.d/nginx-noagent.conf".text = ''
      [Definition]

      failregex = ^<HOST> -.*"-" "-"$

      ignoreregex =
    '';
  };
}
