{ config, pkgs, ... }:

{
  # Basic Seafile service configuration
  services.seafile = {
    enable = true;

    # Admin user configuration
    adminEmail = "admin@cirriform.au";
    initialAdminPassword = "change-this-secure-password"; # Changed only on first setup

    ccnetSettings = {
      General = {
        SERVICE_URL = "https://seafile.cirriform.au";
      };
    };

    # Seafile server settings
    seafileSettings = {
      fileserver = {
        host = "unix:/run/seafile/server.sock";
        web_token_expire_time = 18000; # 5 hours for large uploads
      };

      # Optional: User quotas and cleanup
      quota = {
        default = "50"; # GB allotted to users by default
      };

      history = {
        keep_days = "14"; # Remove deleted files after 14 days
      };
    };

    # Optional: Garbage collection for freed blocks
    gc = {
      enable = true;
      dates = [ "Sun 03:00:00" ]; # Weekly cleanup on Sunday at 3 AM
    };

    dataDir = "/srv/data/seafile/";

    # Optional: Additional Seahub (web interface) configuration
    # seahubExtraConf = ''
    #   # Additional Python Django settings for Seahub
    #   ALLOWED_HOSTS = ['seafile.yourdomain.com', 'localhost']
    # '';
  };

  age.secrets."cf_origin_cert" = {
    file = ../../../secrets/cf_origin_cert.pem.age;
    mode = "770";
    owner = "nginx";
    group = "nginx";
  };

  age.secrets."cf_origin_key" = {
    file = ../../../secrets/cf_origin_key.pem.age;
    mode = "770";
    owner = "nginx";
    group = "nginx";
  };

  # Nginx reverse proxy configuration for Seafile
  services.nginx = {
    enable = true;

    virtualHosts."seafile.cirriform.au" = {
      # SSL Configuration
      forceSSL = true;

      sslCertificate = config.age.secrets."cf_origin_cert".path;
      sslCertificateKey = config.age.secrets."cf_origin_key".path;

      locations = {
        # Main Seahub web interface
        "/" = {
          proxyPass = "http://unix:/run/seahub/gunicorn.sock";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Host $server_name;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_read_timeout 1200s;
            client_max_body_size 0;
          '';
        };

        # File upload/download endpoint
        "/seafhttp" = {
          proxyPass = "http://unix:/run/seafile/server.sock";
          extraConfig = ''
            rewrite ^/seafhttp(.*)$ $1 break;
            client_max_body_size 0;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_connect_timeout 36000s;
            proxy_read_timeout 36000s;
            proxy_send_timeout 36000s;
            send_timeout 36000s;
          '';
        };

        # Optional: Media files (avatars, custom CSS/JS)
        "/media/" = {
          alias = "/var/lib/seafile/seahub-data/media/";
        };
      };
    };
  };

  # Optional: Log rotation for Seafile logs
  services.logrotate.settings.seafile = {
    files = [ "/var/log/seafile/*.log" ];
    frequency = "weekly";
    rotate = 4;
    compress = true;
    delaycompress = true;
    missingok = true;
    notifempty = true;
    create = "644 seafile seafile";
  };
}

# Post-installation notes:
# 1. The initialAdminPassword is set only during first initialization
# 2. To reset admin password: stop service, delete /var/lib/seafile/data,
#    rebuild system, restart service
# 3. Logs are in /var/log/seafile/server.log and /var/log/seafile/seahub.log
# 4. Default data directory is /var/lib/seafile/
# 5. Test the installation at https://seafile.yourdomain.com
# 6. For production, ensure proper backup of /var/lib/seafile/ directory
