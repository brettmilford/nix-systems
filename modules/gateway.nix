{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.gateway;
in
{
  options.services.gateway = {
    enable = mkEnableOption "Enable general reverse proxy config";
  };

  # Override the nginx virtualHosts submodule to set SSL defaults
  options.services.nginx.virtualHosts = mkOption {
    type = types.attrsOf (types.submodule {
      config = {
        forceSSL = mkDefault true;
        enableACME = mkDefault true;
      };
    });
  };

  config = mkIf cfg.enable {

    age.secrets."cert.pem" = {
      file = "${self}/secrets/cf_origin_cert.pem.age";
      mode = "770";
      owner = "nginx";
      group = "nginx";
    };

    age.secrets."key.pem" = {
      file = "${self}/secrets/cf_origin_key.pem.age";
      mode = "770";
      owner = "nginx";
      group = "nginx";
    };

    age.secrets."acme-cf.env" = {
      file = "${self}/secrets/acme-cf.env.age";
    };

    # For vhosts not using cloudflare
    security.acme = {
      acceptTerms = true;
      defaults = {
        email = "certs@cirriform.au";
        dnsProvider = "cloudflare";
        environmentFile = config.age.secrets."acme-cf.env".path;
      };
    };

    services.nginx = {
      enable = true;

      # recommendedProxySettings = true; # NOTE: This often breaks some web apps
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;

      commonHttpConfig =
        let
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
        in
          ''
            ${realIpsFromList cfipv4}
            ${realIpsFromList cfipv6}
            real_ip_header CF-Connecting-IP;

            # Define common rate limits
            limit_req_zone $binary_remote_addr zone=auth:10m rate=30r/m;
            limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;
            limit_req_zone $binary_remote_addr zone=general:10m rate=30r/m;
            limit_req_zone $binary_remote_addr zone=webdav:10m rate=30r/m;
            limit_conn_zone $binary_remote_addr zone=general_conn:10m;

            # From recommendedProxySettings (in the http context)
            proxy_redirect          off;
            proxy_connect_timeout   ${config.services.nginx.proxyTimeout};
            proxy_send_timeout      ${config.services.nginx.proxyTimeout};
            proxy_read_timeout      ${config.services.nginx.proxyTimeout};
            proxy_http_version      1.1;
            proxy_set_header        "Connection" "";
          '';
    };

    # NOTE: cloudflare ban action with preloaded creds.
    age.secrets."cf.conf" = {
      file = "${self}/secrets/fail2ban-cf.conf.age";
      path = "/etc/fail2ban/action.d/cf.conf";
    };

    services.fail2ban = {
      enable = true;
      extraPackages = [pkgs.curl pkgs.ipset pkgs.systemd];
      ignoreIP = [
        "127.0.0.1/8"
        "192.168.0.0/16"
        "172.22.0.0/16"
        "158.180.4.235/32"
      ];

      bantime-increment = {
        enable = true;
        multipliers = "1 2 4 8 16 32 64";
        maxtime = "168h"; # Maximum ban time (1 week)
        overalljails = true;
      };

      jails.nginx-limit-req.settings = {
        enable = true;
        backend = "auto";
        action = ''cf
                   iptables-multiport'';
      };

      jails.nginx-botsearch.settings = {
        enable = true;
        backend = "auto";
        action = ''cf
                   iptables-multiport'';
      };

      jails.nginx-bad-request.settings = {
        enable = true;
        backend = "auto";
        action = ''cf
                   iptables-multiport'';
        logpath = "/var/log/nginx/access.log";
      };

      jails.nginx-forbidden.settings = {
        enable = true;
        backend = "auto";
        action = ''cf
                   iptables-multiport'';
      };

      jails.nginx-noagent =
        ''
        enabled  = true
        port     = http,https
        filter   = nginx-noagent
        backend  = auto
        maxretry = 1
        logpath  = %(nginx_access_log)s
        action   = iptables-multiport
                   cf
        '';
      };

    environment.etc."fail2ban/filter.d/nginx-noagent.conf".text = ''
      [Definition]

      failregex = ^<HOST> -.*"-" "-"$

      ignoreregex =
    '';
  };
}
