{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.immich-oidc;
in
{
  options.services.immich-oidc = {
    enable = mkEnableOption "Immich with basic configuration for OIDC setup";
  };

  config = mkIf cfg.enable {
    age.secrets."immich.json" = {
      file = "${self}/secrets/immich.json.age";
      owner = "immich";
      group = "immich";
    };

    services.immich = {
      enable = true;
      mediaLocation = "/srv/data/immich";
      database.enable = true;
      redis.enable = true;
      environment = {
        IMMICH_MACHINE_LEARNING_ENABLED = "true";
        IMMICH_TRUSTED_PROXIES = "127.0.0.1";
        IMMICH_HOST = lib.mkForce "127.0.0.1";
        IMMICH_CONFIG_FILE = lib.mkForce config.age.secrets."immich.json".path;
      };
    };

    services.nginx = mkIf config.services.nginx.enable {
      virtualHosts."immich.cirriform.au" = {
        forceSSL = true;
        sslCertificate = config.age.secrets."cert.pem".path;
        sslCertificateKey = config.age.secrets."key.pem".path;
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      locations."~ ^/api/(asset/upload|asset/check)" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # Upload-specific settings
            client_max_body_size 5000M;
            client_body_timeout 300s;
            proxy_connect_timeout 300s;
            proxy_send_timeout 300s;
            proxy_read_timeout 300s;
            proxy_request_buffering off;
          '';
        };
      locations."~ ^/api/" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
          proxyWebsockets = true;
          extraConfig = ''
            limit_req zone=api burst=20 nodelay;

            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # API timeouts
            proxy_connect_timeout 60s;
            proxy_send_timeout 60s;
            proxy_read_timeout 60s;

            # Standard file size for API calls
            client_max_body_size 50M;
          '';
        };
      };
    };
    # Create media directory with proper permissions
    systemd.tmpfiles.rules = [
      "d ${config.services.immich.mediaLocation} 0755 immich immich -"
    ];

    services.fail2ban = {
      jails = {
        immich-upload-abuse = ''
          enabled = true
          backend = systemd
          filter = immich-upload-abuse
          maxretry = 10
          findtime = 3600
          bantime = 86400
          action = iptables-multiport[name=immich-upload, port="http,https"]
        '';
      };
    };
    environment.etc = {
      "fail2ban/filter.d/immich-upload-abuse.conf".text = ''
        [Definition]
        failregex = ^.*nginx.*client: <HOST>.*"POST /api/asset/upload.*HTTP.*" 413.*$
                    ^.*nginx.*client: <HOST>.*"POST /api/asset/upload.*HTTP.*" 429.*$
        journalmatch = _SYSTEMD_UNIT=nginx.service
      '';
    };
  };
}
