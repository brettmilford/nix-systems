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
        IMMICH_CONFIG_FILE = lib.mkForce config.age.secrets."immich.json".path;
      };
    };

    services.nginx = {
      proxyTimeout = "600s";
      virtualHosts."immich.cirriform.au" = {
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
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
    # Create media directory with proper permissions
    systemd.tmpfiles.rules = [
      "d ${config.services.immich.mediaLocation} 0755 immich immich -"
    ];

    services.immich.accelerationDevices = null;
    users.users.immich.extraGroups = [ "video" "render" ];
  };
}
