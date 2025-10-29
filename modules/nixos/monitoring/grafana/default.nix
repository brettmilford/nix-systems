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
  services.grafana = {
    settings = {
      server = {
        http_port = cfg.ports.grafana;
        domain = cfg.fqdn;
        root_url = "https://${cfg.fqdn}/";
      };

      # Security settings
      security = {
        admin_user = "admin";
        admin_password = "$__file{${cfg.secretPaths.grafana_pass}}";
        secret_key = "$__file{${cfg.secretPaths.grafana_key}}";
      };

      analytics.reporting_enabled = false;
    };

    # Provision datasources and dashboards
    provision = {
      enable = true;

      datasources.settings = {
        apiVersion = 1;
        datasources = [
          {
            name = "Prometheus";
            type = "prometheus";
            access = "proxy";
            url = "http://localhost:${toString cfg.ports.prometheus}";
            isDefault = true;
          }
          {
            name = "Loki";
            type = "loki";
            access = "proxy";
            url = "http://localhost:${toString cfg.ports.loki}";
          }
        ];
      };

      dashboards.settings = {
        apiVersion = 1;
        providers = [
          {
            name = "default";
            folder = "";
            type = "file";
            options.path = "/var/lib/grafana/dashboards";
          }
        ];
      };
    };
  };

  services.nginx.virtualHosts."${cfg.fqdn}".locations."/" = lib.mkIf cfg.enable {
    proxyPass = "http://localhost:${toString cfg.ports.grafana}/";
    proxyWebsockets = true;
    recommendedProxySettings = true;
  };
}
