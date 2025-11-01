{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.monitoring-unifi;
in
{
  options.services.monitoring-unifi = {
    enable = lib.mkEnableOption "UniFi monitoring (unifi-poller)";

    unifiUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://127.0.0.1:8443";
      description = "UniFi controller URL";
    };

    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Paths to secret files";
    };

    lokiUrl = lib.mkOption {
      type = lib.types.str;
      default = "http://localhost:3100";
      description = "Loki endpoint URL for log collection";
    };
  };

  config = lib.mkIf cfg.enable {
    services.unpoller = {
      enable = true;
      loki.url = cfg.lokiUrl;
      influxdb.disable = true;
      prometheus = {
        http_listen = "0.0.0.0:9130";
        report_errors = false;
      };
      unifi.defaults = {
        url = cfg.unifiUrl;
        verify_ssl = false;
        user = "unifipoller";
        pass = cfg.secretPaths.unifipoller_pass;
        save_sites = true;
        save_dpi = false;
      };
    };

    networking.firewall.allowedTCPPorts = [ 9130 ];
  };
}
