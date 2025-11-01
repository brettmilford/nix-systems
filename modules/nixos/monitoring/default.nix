{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.monitoring;
in
{
  imports = [
    ../gateway.nix
    ./prometheus
    ./alertmanager
    ./grafana
    ./loki
    ./fluentd
    ./snmp-exporter
    ./unpoller
  ];

  options.services.monitoring = {
    enable = lib.mkEnableOption "monitoring stack";

    fqdn = lib.mkOption {
      type = lib.types.str;
      description = "FQDN for monitoring services";
    };

    ports = lib.mkOption {
      type = lib.types.attrsOf lib.types.port;
      default = {
        prometheus = 9090;
        nodeExporter = 9100;
        loki = 3100;
        alertmanager = 9093;
        grafana = 3000;
      };
      description = "Port configuration for monitoring services";
    };

    targets = lib.mkOption {
      type = lib.types.attrs;
      description = "Monitoring targets";
    };

    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      description = "Secret file paths";
    };

    enableAlertManager = lib.mkEnableOption "Add Alert manager";
  };

  config = lib.mkIf cfg.enable {
    services.gateway.enable = true;
    # Monitoring stack
    services.prometheus.enable = true;
    services.prometheus.alertmanager.enable = cfg.enableAlertManager;
    services.grafana.enable = true;
    # Log stack
    services.loki.enable = true;
    services.fluentd.enable = true;
    # Exporters
    services.prometheus.exporters.snmp.enable = true;
  };
}
