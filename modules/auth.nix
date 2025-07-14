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
        hostname = cfg.domain;
        http-enabled = true;
        http-port = 8080;
        proxy-headers = "xforwarded";
      };
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
        forceSSL = true;
        sslCertificate = config.age.secrets."cert.pem".path;
        sslCertificateKey = config.age.secrets."key.pem".path;
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.keycloak.settings.http-port}";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      };
    };
}
