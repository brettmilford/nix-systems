{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.monitoring-node;
in
{
  options.services.monitoring-node = {
    enable = lib.mkEnableOption "node exporter client for monitoring";

    port = lib.mkOption {
      type = lib.types.port;
      default = 9100;
      description = "Port for node exporter";
    };

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "Address to listen on";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Open firewall for node exporter";
    };
  };

  config = lib.mkIf cfg.enable {
    # nix eval --json '.#nixosConfigurations.orpheus.config.services.prometheus.exporters.node' | jq
    services.prometheus.exporters.node = {
      enable = true;
      port = cfg.port;
      listenAddress = cfg.listenAddress;
      openFirewall = cfg.openFirewall;
      enabledCollectors = [
        "systemd"
        "filesystem"
        "meminfo"
        "loadavg"
        "stat"
        "processes"
        "interrupts"
        "nfs"
      ];
    };
  };
}
