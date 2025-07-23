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

    # Use http challenge
    security.acme.certs."immich.cirriform.au".dnsProvider = null;
    services.nginx = {
      proxyTimeout = "600s";
      virtualHosts."immich.cirriform.au" = {
        enableACME = true;
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

    services.fail2ban = {
      jails.immich.settings = {
        enabled = true;
        filter = "immich";
        backend = "systemd";
      };
    };

    environment.etc."fail2ban/filter.d/immich.local" = {
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
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:rx ${config.services.immich.mediaLocation}/library
        ${pkgs.acl}/bin/setfacl -d -m u:nextcloud:rx ${config.services.immich.mediaLocation}/library

        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv/data
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x ${config.services.immich.mediaLocation}
      '';
    };
  };
}
