{
  self,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.services.cloud;
in
{
  imports = [
    ./gateway.nix
  ];

  options.services.cloud = {
    enable = mkEnableOption "Nextcloud";
    enableOffice = mkEnableOption "Collabora";
    enableFlow = mkEnableOption "Nextcloud flow/windmill";
    dataPath = mkOption {
      type = types.str;
      default = "/srv/data/nextcloud";
      description = "Directory where paperless data are stored";
    };
    fqdn = mkOption {
      type = types.str;
      example = "cloud.example.com";
      description = "Domain name for Nextcloud instance";
    };
    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      description = "Secret file paths";
    };
  };

  config = mkIf cfg.enable {
    services.nextcloud = {
      enable = true;
      package = pkgs.nextcloud32;
      hostName = "${cfg.fqdn}";
      config = {
        dbtype = "pgsql";
        adminpassFile = cfg.secretPaths.nextcloud-admin-passwd;
        dbhost = "/run/postgresql";
      };
      database.createLocally = true;
      appstoreEnable = false;
      extraApps = {
        inherit (config.services.nextcloud.package.packages.apps)
          oidc_login
          calendar
          contacts
          richdocuments
          ;
      };
      extraAppsEnable = true;
      enableImagemagick = true;
      configureRedis = true;
      https = true;
      datadir = "${cfg.dataPath}";
      settings = {
        mail_smtpmode = "sendmail";
        mail_sendmailmode = "pipe";
        mail_from_address = "admin";
        mail_domain = "cirriform.au";
        enable_previews = true;
        enabledPreviewProviders = [
          "OC\\Preview\\PNG"
          "OC\\Preview\\JPEG"
          "OC\\Preview\\GIF"
          "OC\\Preview\\BMP"
          "OC\\Preview\\XBitmap"
          "OC\\Preview\\MP3"
          "OC\\Preview\\TXT"
          "OC\\Preview\\MarkDown"
          "OC\\Preview\\OpenDocument"
          "OC\\Preview\\Krita"
          "OC\\Preview\\HEIC"
          "OC\\Preview\\PDF"
          "OC\\Preview\\Movie"
          "OC\\Preview\\MKV"
          "OC\\Preview\\MP4"
          "OC\\Preview\\AVI"
          "OC\\Preview\\MSOfficeDoc"
        ];
        trusted_proxies = [
          "127.0.0.1"
          "::1"
        ];
        overwriteprotocol = "https";
        default_phone_region = "AU";
        "overwrite.cli.url" = "https://cloud.cirriform.au";
        overwritehost = "${cfg.fqdn}";
        forwarded_for_headers = [ "X-Forwarded-For" ];
        oidc_login_client_id = "nextcloud";
        # TODO: get from catalog
        oidc_login_provider_url = "https://auth.cirriform.au/realms/master";
        oidc_login_auto_redirect = true;
        oidc_login_redir_fallback = true;
        oidc_login_end_session_redirect = true;
        oidc_login_logout_url = "https://${cfg.fqdn}/apps/oidc_login/oidc";
        oidc_login_attributes = {
          id = "preferred_username";
          mail = "email";
        };
        oidc_login_code_challenge_method = "S256";
        oidc_login_hide_password_form = true;
      };
      secretFile = cfg.secretPaths."nextcloud-secrets.json";
      phpOptions = {
        catch_workers_output = "yes";
        display_errors = "stderr";
        error_reporting = "E_ALL & ~E_DEPRECATED & ~E_STRICT";
        expose_php = "Off";
        "opcache.enable_cli" = "1";
        "opcache.fast_shutdown" = "1";
        "opcache.interned_strings_buffer" = "16";
        "opcache.max_accelerated_files" = "10000";
        "opcache.memory_consumption" = "128";
        "opcache.revalidate_freq" = "1";
        "openssl.cafile" = "/etc/ssl/certs/ca-certificates.crt";
        short_open_tag = "Off";
      };
    };

    services.gateway.enable = true;

    services.fail2ban.jails.nextcloud-auth.settings = {
      enabled = true;
      filter = "nextcloud-auth";
      backend = "systemd";
      journalmatch = "SYSLOG_IDENTIFIER=Nextcloud + PRIORITY=4";
      action = ''
        cf
                         iptables-allports'';
    };

    environment.etc."fail2ban/filter.d/nextcloud-auth.local" = {
      text = ''
        [Definition]
        failregex = "remoteAddr":\s*"<HOST>".*"message":\s*"Login failed:
        maxlines = 1

        ignoreregex =
      '';
    };

    environment.systemPackages = with pkgs; [
      imagemagick
      ffmpeg_6-headless
      libde265
      libheif
    ];

    systemd.services.nextcloud-acl-setup = {
      description = "Setup base nextcloud acls for external storage";
      after = [ "systemd-tmpfiles-setup.service" ];
      wants = [ "systemd-tmpfiles-setup.service" ];
      before = [ "nextcloud-file-scan.service" ];
      wantedBy = [ "nextcloud-file-scan.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
      };

      script = ''
        echo "Setting nextcloud base ACLs"
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv/data
      '';
    };

    systemd.services.nextcloud-file-scan = {
      description = "Scan nextcloud external storage";
      requires = [ "nextcloud-setup.service" ];
      after = [
        "systemd-tmpfiles-setup.service"
        "paperless-exporter.service"
        "nextcloud-acl-setup.service"
        "nextcloud-setup.service"
      ];
      wants = [
        "systemd-tmpfiles-setup.service"
        "paperless-exporter.service"
        "nextcloud-acl-setup.service"
      ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
      };

      script = ''
        echo "Running nextcloud files scan"
        # see nextcloud-occ files_external:list
        # Immich Library - brett
        # ${config.services.nextcloud.occ}/bin/nextcloud-occ files_external:scan 8 -v
        # Paperless export - brett
        ${config.services.nextcloud.occ}/bin/nextcloud-occ files_external:scan 9 -v
        # Paperless export - kate
        ${config.services.nextcloud.occ}/bin/nextcloud-occ files_external:scan 10 -v

      '';
    };

    services.collabora-online = mkIf cfg.enableOffice {
      enable = true;
      settings = {
        ssl = {
          enable = false;
          termination = true;
        };

        net = {
          listen = "lookback";
          post_allow.host = [
            "127.0.0.1"
            "::1"
          ];
        };

        sotrage.wopi = {
          "@allow" = true;
          host = [ "cloud.cirriform.au" ];
        };

        server_name = "collabora.cirriform.au";
      };
    };

    # TODO: get from catalog
    services.nginx.virtualHosts."collabora.cirriform.au" = mkIf cfg.enableOffice {
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.collabora-online.port}";
        proxyWebsockets = true;
      };
    };

    services.windmill = mkIf cfg.enableFlow {
      enable = true;
      baseUrl = "https://windmill.cirriform.au";
    };

    services.nginx.virtualHosts."windmill.cirriform.au" = mkIf cfg.enableFlow {
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.windmill.serverPort}";
      };
    };
  };
}
