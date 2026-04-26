{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.ftp;
  certDir = config.security.acme.certs.${cfg.fqdn}.directory;
in
{
  imports = [
    ./users.nix
  ];

  options.services.ftp = {
    enable = mkEnableOption "FTP server (vsftpd with FTPS)";
    fqdn = mkOption {
      type = types.str;
      description = "FQDN for the FTP server (used for ACME cert)";
    };
    dataPath = mkOption {
      type = types.str;
      description = "Root directory for FTP data";
    };
    pasvMinPort = mkOption {
      type = types.port;
      default = 40000;
      description = "Minimum passive mode port";
    };
    pasvMaxPort = mkOption {
      type = types.port;
      default = 40100;
      description = "Maximum passive mode port";
    };
    lanAddress = mkOption {
      type = types.str;
      description = "LAN IP address for passive mode responses";
    };
  };

  config = mkIf cfg.enable {
    security.acme.certs.${cfg.fqdn} = {
      dnsProvider = "cloudflare";
      environmentFile = config.age.secrets."acme-cf.env".path;
      group = "vsftpd";
      keyType = "rsa2048";
    };

    services.vsftpd = {
      enable = true;
      localUsers = true;
      anonymousUser = false;
      writeEnable = true;
      chrootlocalUser = true;
      allowWriteableChroot = true;
      userlistEnable = true;
      userlistDeny = false;
      userlist = [ "reolink" ];

      forceLocalLoginsSSL = true;
      forceLocalDataSSL = true;
      rsaCertFile = "${certDir}/fullchain.pem";
      rsaKeyFile = "${certDir}/key.pem";

      extraConfig = ''
        pasv_enable=YES
        pasv_min_port=${toString cfg.pasvMinPort}
        pasv_max_port=${toString cfg.pasvMaxPort}
        pasv_address=${cfg.lanAddress}
        local_root=${cfg.dataPath}
        require_ssl_reuse=NO
        local_umask=022
      '';
    };

    systemd.tmpfiles.rules = [
      "d ${cfg.dataPath} 0755 root root -"
      "d ${cfg.dataPath}/reolink 0755 reolink reolink -"
    ];

    networking.firewall.allowedTCPPorts = [ 21 ];
    networking.firewall.allowedTCPPortRanges = [
      {
        from = cfg.pasvMinPort;
        to = cfg.pasvMaxPort;
      }
    ];
  };
}
