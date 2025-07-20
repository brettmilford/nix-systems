{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.auth;
in
{
  options.services.auth = {
    enable = mkEnableOption "Custom Keycloak setup";

    domain = mkOption {
      type = types.str;
      example = "auth.example.com";
      description = "Domain name for Keycloak instance";
    };
  };

  config = mkIf cfg.enable {
    services.postgresql.enable = true;

    age.secrets.keycloak-db-passwd = {
      file = "${self}/secrets/keycloak-db-passwd.age";
    };

    # Configure Keycloak service
    services.keycloak = {
      enable = true;
      initialAdminPassword = "temporaryAdminPassword";

      database = {
        type = "postgresql";
        createLocally = true;
        username = "keycloak";
        passwordFile = config.age.secrets.keycloak-db-passwd.path;
      };

      settings = {
        hostname = "https://auth.cirriform.au";
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

    services.nginx = {
      virtualHosts."auth.cirriform.au" = {
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
    services.fail2ban = {
      enable = true;
      jails = {
        keycloak-auth = ''
          enabled = true
          backend = systemd
          filter = keycloak-auth
          maxretry = 5
          findtime = 600
          bantime = 3600
          action = iptables-multiport[name=keycloak-auth, port="http,https"]
        '';
        keycloak-admin = ''
          enabled = true
          backend = systemd
          filter = keycloak-admin
          maxretry = 3
          findtime = 300
          bantime = 86400
          action = iptables-multiport[name=keycloak-admin, port="http,https"]
        '';
      };
    };

    # Fail2ban filters for Keycloak
    environment.etc = {
      "fail2ban/filter.d/keycloak-auth.conf".text = ''
        [Definition]
        failregex = ^.*keycloak.*WARN.*type=LOGIN_ERROR.*user=.* error=invalid_user_credentials.*ip=<HOST>.*$
                    ^.*keycloak.*WARN.*type=LOGIN_ERROR.*user=.* error=user_not_found.*ip=<HOST>.*$
                    ^.*keycloak.*WARN.*type=LOGIN_ERROR.*user=.* error=user_disabled.*ip=<HOST>.*$
        journalmatch = _SYSTEMD_UNIT=keycloak.service
      '';

      "fail2ban/filter.d/keycloak-admin.conf".text = ''
        [Definition]
        failregex = ^.*nginx.*client: <HOST>.*"(GET|POST).*/admin/.*HTTP.*" (40[1-4]|50[0-3]).*$
        journalmatch = _SYSTEMD_UNIT=nginx.service
      '';

    };
  };
}
