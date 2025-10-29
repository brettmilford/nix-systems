{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.auth;
in
{
  imports = [
    ./gateway.nix
  ];

  options.services.auth = {
    enable = mkEnableOption "Custom Keycloak setup";

    fqdn = mkOption {
      type = types.str;
      example = "auth.example.com";
      description = "Domain name for Keycloak instance";
    };
    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      description = "Secret file paths";
    };
  };

  config = mkIf cfg.enable {
    services.postgresql.enable = true;

    # Configure Keycloak service
    services.keycloak = {
      enable = true;
      initialAdminPassword = "temporaryAdminPassword";

      database = {
        type = "postgresql";
        createLocally = true;
        username = "keycloak";
        passwordFile = cfg.secretPaths.keycloak-db-passwd;
      };

      settings = {
        hostname = "https://${cfg.fqdn}";
        # TODO: get from catalog
        hostname-admin = "https://keycloak.cirriform.au";
        http-enabled = true;
        http-port = 8080;
        proxy-headers = "xforwarded";
        spi-login-protocol-openid-connect-legacy-logout-redirect-uri = "false";
        log-level = "INFO";
        log-console-format = "%d{yyyy-MM-dd HH:mm:ss,SSS} %-5p [%c] (%t) %s%e%n";
        http-max-queued-requests = "1000";
        Xms = "1g";
        Xmx = "2g";
      };
    };

    services.gateway.enable = true;
    services.nginx = {
      virtualHosts."${cfg.fqdn}" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.keycloak.settings.http-port}";
          proxyWebsockets = true;
          recommendedProxySettings = true;
        };
      };
      virtualHosts."keycloak.cirriform.au" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.keycloak.settings.http-port}";
          proxyWebsockets = true;
          recommendedProxySettings = true;
        };
      };
    };

    services.fail2ban.jails.keycloak-auth.settings = {
      enabled = true;
      port = "http,https";
      filter = "keycloak-auth";
      backend = "systemd";
      action = ''cf
                 iptables-multiport'';
    };

    environment.etc."fail2ban/filter.d/keycloak-auth.local" = {
      text = ''
        [INCLUDES]
        before = common.conf

        [Definition]
        failregex = .*LOGIN_ERROR.*ipAddress="<HOST>".*(user_not_found|invalid_user_credentials).*

        [Init]
        journalmatch = _SYSTEMD_UNIT=keycloak.service
      '';
    };

  };
}
