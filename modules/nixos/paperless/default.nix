{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.paperless-ngx;
  pythonWithPackages = pkgs.python3.withPackages (ps: with ps; [
    httpx
  ]);
  postConsumeScript = pkgs.writeScriptBin "post-consume-script" ''
    #!${pythonWithPackages}/bin/python3
    ${builtins.readFile ./post-consume-script.py}
  '';
in

{

  imports = [
    ../gateway.nix
  ];

  options.services.paperless-ngx = {
    enable = mkEnableOption "Paperless-NGX document management system";
    dataDir = mkOption {
      type = types.str;
      default = "/srv/data/paperless";
      description = "Directory where paperless data are stored";
    };
    fqdn = mkOption {
      type = types.str;
      example = "paperless.example.com";
      description = "Domain name for Paperless instance";
    };
    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      description = "Secret file paths";
    };
  };

  config = mkIf cfg.enable {

    services.gotenberg.port = 3200;

    services.paperless = {
      enable = true;
      address = "127.0.0.1";
      port = 28981;
      passwordFile = cfg.secretPaths.paperless-admin-passwd;
      configureTika = true;
      database.createLocally = true;
      environmentFile = cfg.secretPaths."paperless.env";
      exporter = {
        enable = true;
        settings = {
          use-filename-format = true;
        };
      };


      settings = {
        TMPDIR = "${cfg.dataDir}/tmp";
        TEMP = "${cfg.dataDir}/tmp";
        TMP = "${cfg.dataDir}/tmp";
        PAPERLESS_SCRATCH_DIR = "${cfg.dataDir}/tmp";
        PAPERLESS_CONVERT_TMPDIR = "${cfg.dataDir}/tmp";

        PAPERLESS_URL = "https://${cfg.fqdn}";
        PAPERLESS_ALLOWED_HOSTS = "${cfg.fqdn}";
        PAPERLESS_TRUSTED_PROXIES = "127.0.0.1,::1,localhost";
        PAPERLESS_USE_X_FORWARD_HOST = "true";
        PAPERLESS_PROXY_SSL_HEADER = [ "HTTP_X_FORWARDED_PROTO" "https"];

        PAPERLESS_OCR_MAX_IMAGE_PIXELS = 500000000;
        PAPERLESS_OCR_LANGUAGE = "eng";
        PAPERLESS_OCR_MODE = "redo";
        PAPERLESS_CONSUMER_RECURSIVE = "true";
        PAPERLESS_TASK_WORKERS = "2";
        PAPERLESS_THREADS_PER_WORKER = "1";
        PAPERLESS_OCR_USER_ARGS = {
          "continue_on_soft_render_error" = true;
        };

        PAPERLESS_TIKA_GOTENBERG_ENDPOINT = "http://localhost:${toString config.services.gotenberg.port}";

        PAPERLESS_EMAIL_PORT = 25;
        PAPERLESS_EMAIL_HOST = "127.0.0.1";
        PAPERLESS_EMAIL_FROM = "admin@cirriform.au";

        PAPERLESS_APPS = "allauth.socialaccount.providers.openid_connect";
        PAPERLESS_SOCIAL_AUTO_SIGNUP = "true";
        PAPERLESS_ACCOUNT_DEFAULT_GROUPS = "user";
        PAPERLESS_SOCIAL_ACCOUNT_SYNC_GROUPS = "true";
        PAPERLESS_REDIRECT_LOGIN_TO_SSO = "true";

        PAPERLESS_POST_CONSUME_SCRIPT = "${postConsumeScript}/bin/post-consume-script";
        PAPERLESS_EMAIL_PARSE_DEFAULT_LAYOUT = 2;
        PAPERLESS_FILENAME_FORMAT_REMOVE_NONE = "true";
      };

      dataDir = "${cfg.dataDir}";
      mediaDir = "${cfg.dataDir}/media";
      consumptionDir = "${cfg.dataDir}/consume";
    };

    systemd.services.paperless-consumer.serviceConfig.PrivateTmp = lib.mkForce false;
    systemd.services.paperless-task-queue.serviceConfig.PrivateTmp = lib.mkForce false;

    systemd.tmpfiles.rules = [
      "d ${cfg.dataDir}/tmp 0750 paperless paperless -"
      "d ${cfg.dataDir}/consume 0750 paperless paperless -"
      "d ${cfg.dataDir}/export 0750 paperless paperless -"
      # per-user dirs
      "d ${cfg.dataDir}/consume/brett 0750 paperless paperless -"
      "d ${cfg.dataDir}/consume/kate 0750 paperless paperless -"
    ];

    systemd.services.paperless-facl-setup = {
      description = "Set up Paperless directory ACLs";
      after = [ "systemd-tmpfiles-setup.service" ];
      wants = [ "systemd-tmpfiles-setup.service" ];
      before = [ "nextcloud-file-scan.service" ];
      wantedBy = [ "nextcloud-file-scan.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
      };

      script = ''
        echo "Setting nextcloud-paperless base ACLs"
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x "${cfg.dataDir}"

        echo "Setting nextcloud-paperless rw ACLs"
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:rwx ${cfg.dataDir}/consume
        ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rwx ${cfg.dataDir}/consume
        ${pkgs.acl}/bin/setfacl -R -m u:nextcloud:rwx,mask:rwx ${cfg.dataDir}/consume

        echo "Setting nextcloud-paperless ro ACLs"
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:rx ${cfg.dataDir}/export
        ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rx ${cfg.dataDir}/export
        ${pkgs.acl}/bin/setfacl -R -m u:nextcloud:rx,mask:rx ${cfg.dataDir}/export
      '';
    };

    services.gateway.enable = true;
    services.nginx = mkIf config.services.nginx.enable {
      virtualHosts."${cfg.fqdn}" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:28981";
          recommendedProxySettings = true;
          extraConfig = ''
            client_max_body_size 100M;
          '';
        };
        # Block django admin
        locations."/admin/" = {
          extraConfig = ''
            deny all;
            return 403;
          '';
        };
      };
    };
  };
}
