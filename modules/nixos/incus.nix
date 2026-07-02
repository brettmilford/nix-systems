{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.incus;
in
{
  options.services.incus = {
    enable = mkEnableOption "Incus container and VM management";

    dataPath = mkOption {
      type = types.str;
      default = "/var/lib/incus";
      description = "Directory for Incus storage pools";
    };

    bridgeNetwork = mkOption {
      type = types.str;
      default = "10.0.100.1/24";
      description = "CIDR for the Incus bridge network";
    };

    defaultProfileDiskSize = mkOption {
      type = types.str;
      default = "10GiB";
      description = "Default disk size for the default profile";
    };
  };

  config = mkIf cfg.enable {
    virtualisation.incus = {
      enable = true;
      preseed = {
        networks = [
          {
            name = "incusbr0";
            type = "bridge";
            config = {
              "ipv4.address" = cfg.bridgeNetwork;
              "ipv4.nat" = "true";
            };
          }
        ];
        storage_pools = [
          {
            name = "default";
            driver = "dir";
            config = {
              source = "${cfg.dataPath}/storage-pools/default";
            };
          }
        ];
        profiles = [
          {
            name = "default";
            devices = {
              eth0 = {
                name = "eth0";
                network = "incusbr0";
                type = "nic";
              };
              root = {
                path = "/";
                pool = "default";
                size = cfg.defaultProfileDiskSize;
                type = "disk";
              };
            };
          }
        ];
      };
    };
  };
}
