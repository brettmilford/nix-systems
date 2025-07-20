{ self, config, lib, ... }:

with lib;

let
  cfg = config.services.mealie-oidc;
in
{
  options.services.mealie-oidc = {
    enable = mkEnableOption "Mealie";
  };

  config = mkIf cfg.enable {
    age.secrets."mealie.env" = {
      file = "${self}/secrets/mealie.env.age";
    };

    services.mealie = {
      enable = true;
      database.createLocally = true;
      listenAddress = "127.0.0.1";
      credentialsFile = config.age.secrets."mealie.env".path;
      settings = {
        SMTP_HOST = "127.0.0.1";
        SMTP_PORT = 25;
        SMTP_AUTH_STRATEGY = "NONE";
        SMTP_FROM_EMAIL = "admin@cirriform.au";
        OIDC_AUTH_ENABLED = "true";
        OIDC_SIGNUP_ENABLED = "true";
        OIDC_CONFIGURATION_URL = "https://auth.cirriform.au/realms/master/.well-known/openid-configuration";
        OIDC_CLIENT_ID = "mealie";
        OIDC_PROVIDER_NAME = "Keycloak";
        OIDC_AUTO_REDIRECT = "true";
        OIDC_USER_CLAIM = "preferred_username";
        OIDC_REMEMBER_ME = "false";
        OIDC_GROUPS_CLAIM = "groups";
        OIDC_USER_GROUP = "users";
        OIDC_ADMIN_GROUP = "admins";
        ALLOW_SIGNUP = "false";
      };
    };
    systemd.services.mealie = {
      serviceConfig = {
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        SystemCallFilter = [ "@system-service" "~@privileged" ];
        IPAddressDeny = [
          "10.0.0.0/8"
          "172.16.0.0/12"
          "192.168.0.0/16"
          "169.254.0.0/16"
          "127.0.0.0/8"
        ];
        IPAddressAllow = [
          "127.0.0.1"
        ];
      };
    };

    services.nginx = {
      virtualHosts."mealie.cirriform.au" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:9000";
          recommendedProxySettings = true;
          extraConfig = ''
            client_max_body_size 100M;
          '';
        };
        locations."/api/recipes/create-url" = {
          proxyPass = "http://127.0.0.1:9000";
          recommendedProxySettings = true;
          extraConfig = ''
            limit_req zone=api burst=20 nodelay;
          '';
        };
        locations."~ ^/(api/admin|api/debug)" = {
          extraConfig = ''
            deny all;
            return 404;
          '';
        };
      };
    };
  };
}
