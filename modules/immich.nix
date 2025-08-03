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

    environment.systemPackages = with pkgs; [ exiftool ];

    services.immich = {
      enable = true;
      accelerationDevices = null;
      mediaLocation = "/srv/data/immich";
      database.enable = true;
      redis.enable = true;
      environment = {
        IMMICH_CONFIG_FILE = lib.mkForce config.age.secrets."immich.json".path;
      };
      machine-learning.environment = {
        MACHINE_LEARNING_CACHE_FOLDER = lib.mkForce "/srv/data/immich/cache";
      };
    };

    users.users.immich.extraGroups = [ "video" "render" ];

    services.nginx = {
      proxyTimeout = "600s";
      virtualHosts."immich.cirriform.au" = {
        enableACME = true;
        extraConfig = ''
          access_log /var/log/nginx/immich.access.log;
        '';
        locations."/" = {
          proxyPass = "http://[::1]:${toString config.services.immich.port}";
          proxyWebsockets = true;
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_read_timeout 600s;
            proxy_send_timeout 600s;
            send_timeout       600s;
            client_max_body_size 50000M;
          '';
        };
      };
    };

    services.fail2ban.jails.immich-auth.settings = {
      enabled = true;
      port = "http,https";
      filter = "immich-auth";
      backend = "systemd";
      action = ''cf
                 iptables-multiport'';
    };

    environment.etc."fail2ban/filter.d/immich-auth.local" = {
      text = ''
        [Definition]
        failregex = .*immich\[[0-9]+\]:.*Failed login attempt for user.+from ip address\s+<ADDR>

        [Init]
        journalmatch = _SYSTEMD_UNIT=immich-server.service
      '';
    };

    # Create media directory with proper permissions
    systemd.tmpfiles.rules = [
      "d ${config.services.immich.mediaLocation} 0755 immich immich -"
    ];

    systemd.services.immich-facl-setup = {
      description = "Set up Immich directory ACLs";
      after = [ "systemd-tmpfiles-setup.service" ];
      wants = [ "systemd-tmpfiles-setup.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };

      script = ''
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv/data
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x ${config.services.immich.mediaLocation}
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:rx ${config.services.immich.mediaLocation}/library
        ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rx ${config.services.immich.mediaLocation}/library
        ${pkgs.acl}/bin/setfacl -R -m u:nextcloud:rX ${config.services.immich.mediaLocation}/library
        ${pkgs.findutils}/bin/find ${config.services.immich.mediaLocation}/library -type d -exec ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rX {} \;
        ${pkgs.findutils}/bin/find ${config.services.immich.mediaLocation}/library \
              -type f ! -newermt "1970-01-03" ! -iname "*.mov" ! -iname "*.mp4" \
              -exec ${pkgs.exiftool}/bin/exiftool -overwrite_original -q \
                "-FileModifyDate<DateTimeOriginal" {} \;
        ${pkgs.findutils}/bin/find ${config.services.immich.mediaLocation}/library \
          -iname "*.mov" -type f ! -newermt "1970-01-03"
          -exec ${pkgs.exiftool}/bin/exiftool  -overwrite_original -q
            "-FileModifyDate<CreationDate" {} \;
        ${pkgs.findutils}/bin/find ${config.services.immich.mediaLocation}/library \
          -iname "*.mp4" -type f ! -newermt "1970-01-03"
          -exec ${pkgs.exiftool}/bin/exiftool  -overwrite_original -q
            "-FileModifyDate<CreateDate" {} \;
        ${config.services.nextcloud.occ}/bin/nextcloud-occ files_external:scan 8 -v
      '';
    };
  };
}
