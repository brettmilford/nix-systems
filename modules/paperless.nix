{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.paperless-ngx;
in

{
  options.services.paperless-ngx = {
    enable = mkEnableOption "Paperless-NGX document management system";
    dataDir = mkOption {
      type = types.str;
      default = "/srv/data/paperless";
      description = "Directory where paperless data are stored";
    };
  };

  config = mkIf cfg.enable {

    age.secrets.admin_pass = {
      file = "${self}/secrets/nextcloud.age";
    };

    age.secrets."paperless.env" = {
      file = "${self}/secrets/paperless.env.age";
    };

    age.secrets."paperlessBackupPass" = {
      file = "${self}/secrets/paperlessBackupPass.age";
    };

    services.gotenberg.port = 3200;

    services.paperless = {
      enable = true;
      address = "127.0.0.1";
      port = 28981;
      passwordFile = config.age.secrets.admin_pass.path;
      configureTika = true;
      database.createLocally = true;
      environmentFile = config.age.secrets."paperless.env".path;
      exporter.enable = true;

      settings = {
        TMPDIR = "${cfg.dataDir}/tmp";
        TEMP = "${cfg.dataDir}/tmp";
        TMP = "${cfg.dataDir}/tmp";
        PAPERLESS_SCRATCH_DIR = "${cfg.dataDir}/tmp";
        PAPERLESS_CONVERT_TMPDIR = "${cfg.dataDir}/tmp";

        PAPERLESS_URL = "https://paperless.cirriform.au";
        PAPERLESS_ALLOWED_HOSTS = "paperless.cirriform.au";
        PAPERLESS_TRUSTED_PROXIES = "127.0.0.1";
        #PAPERLESS_OAUTH_CALLBACK_BASE_URL = "https://paperless.cirriform.au/"

        PAPERLESS_OCR_LANGUAGE = "eng";
        PAPERLESS_CONSUMER_RECURSIVE = "true";
        PAPERLESS_CONSUMER_SUBDIRS_AS_TAGS = "true";
        PAPERLESS_TASK_WORKERS = "2";
        PAPERLESS_THREADS_PER_WORKER = "1";

        PAPERLESS_TIKA_GOTENBERG_ENDPOINT = "http://localhost:${toString config.services.gotenberg.port}";

        # PAPERLESS_FILENAME_FORMAT = "{created_year}/{correspondent}/{title}";
        #PAPERLESS_ENABLE_NLTK = "true";
      };

      dataDir = "${cfg.dataDir}";
      mediaDir = "${cfg.dataDir}/media";
      consumptionDir = "${cfg.dataDir}/consume";
    };

    systemd.services.paperless-consumer.serviceConfig.PrivateTmp = lib.mkForce false;
    systemd.services.paperless-task-queue.serviceConfig.PrivateTmp = lib.mkForce false;

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir}/tmp 0750 paperless paperless -"
    ];

    systemd.services.paperless-facl-setup = {
      description = "Set up Paperless directory ACLs";
      after = [ "systemd-tmpfiles-setup.service" ];
      wants = [ "systemd-tmpfiles-setup.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };

      script = ''
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:rwx ${cfg.dataDir}/consume
        ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rwx ${cfg.dataDir}/consume

        ${pkgs.acl}/bin/setfacl -m u:nextcloud:rx ${cfg.dataDir}/export
        ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rx ${cfg.dataDir}/export

        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv/data
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv/data/paperless
      '';
    };

    services.nginx = mkIf config.services.nginx.enable {
      virtualHosts."paperless.cirriform.au" = {
        forceSSL = true;
        sslCertificate = config.age.secrets."cert.pem".path;
        sslCertificateKey = config.age.secrets."key.pem".path;
        locations."/" = {
          proxyPass = "http://127.0.0.1:28981";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            client_max_body_size 100M;
          '';
        };
      };
    };

    # systemctl --user status protonmail-bridge
    services.protonmail-bridge.enable = true;
  };
}
