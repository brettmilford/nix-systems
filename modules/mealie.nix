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
        forceSSL = true;
        sslCertificate = config.age.secrets."cert.pem".path;
        sslCertificateKey = config.age.secrets."key.pem".path;
        locations."/" = {
          proxyPass = "http://127.0.0.1:9000";
          extraConfig = ''
            # Standard proxy headers
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # File upload limit
            client_max_body_size 100M;


            # Timeout settings
            proxy_connect_timeout 5s;
            proxy_send_timeout 60s;
            proxy_read_timeout 60s;

            # Buffer settings
            proxy_buffering on;
            proxy_buffer_size 4k;
            proxy_buffers 8 4k;
          '';
        };
        locations."/api/recipes/create-url" = {
          proxyPass = "http://127.0.0.1:9000";
          extraConfig = ''
            limit_req zone=api burst=20 nodelay;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
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
