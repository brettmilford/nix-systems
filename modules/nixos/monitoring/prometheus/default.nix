{ config, lib, pkgs, ... }:
let
  cfg = config.services.monitoring;
in
{
    # Prometheus configuration
    services.prometheus = {
      port = cfg.ports.prometheus;
      listenAddress = "localhost";
      webExternalUrl = "http://${cfg.fqdn}/prometheus";
      extraFlags = [ "--web.route-prefix=/" ];
      # NOTE: configCheck breaks passing secret paths for authorization if they don't exist before activation
      checkConfig = "syntax-only";

      globalConfig = {
        scrape_interval = "15s";
        evaluation_interval = "15s";
      };

      # Alert rules - basic system monitoring
      rules = [
        (builtins.toJSON {
          groups = [
            {
              name = "basic-monitoring";
              rules = [
                {
                  alert = "InstanceDown";
                  expr = "up == 0";
                  for = "5m";
                  labels.severity = "critical";
                  annotations = {
                    summary = "Instance {{ $labels.instance }} down";
                    description = "{{ $labels.instance }} has been down for more than 5 minutes.";
                  };
                }
                {
                  alert = "HighCPUUsage";
                  expr = "100 - (avg by(instance) (irate(node_cpu_seconds_total{mode=\"idle\"}[5m])) * 100) > 80";
                  for = "5m";
                  labels.severity = "warning";
                  annotations = {
                    summary = "High CPU usage on {{ $labels.instance }}";
                    description = "CPU usage is above 80% for more than 5 minutes.";
                  };
                }
                {
                  alert = "HighMemoryUsage";
                  expr = "(1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100 > 85";
                  for = "5m";
                  labels.severity = "warning";
                  annotations = {
                    summary = "High memory usage on {{ $labels.instance }}";
                    description = "Memory usage is above 85% for more than 5 minutes.";
                  };
                }
              ];
            }
          ];
        })
      ];

      # Scrape configurations
      # NOTE: Get the generated config with:
      # nix eval --json '.#nixosConfigurations.eurydice.config.services.prometheus.scrapeConfigs' --apply 'configs: map (c: { job_name = c.job_name; targets = if c ? static_configs then c.static_configs else "no_targets"; }) configs' | jq
      scrapeConfigs = [
        {
          job_name = "prometheus";
          static_configs = [
            {
              targets = [ "localhost:${toString cfg.ports.prometheus}" ];
            }
          ];
        }
        {
          job_name = "loki";
          static_configs = [
            {
              targets = [ "localhost:${toString cfg.ports.loki}" ];
            }
          ];
        }
        {
          job_name = "grafana";
          static_configs = [
            {
              targets = [ "localhost:${toString cfg.ports.grafana}" ];
            }
          ];
        }
        {
          job_name = "node-exporter";
          static_configs = map (target: {
            targets = [ "${target.ip}:${toString target.port}" ];
            labels = {
              hostname = target.hostname;
            };
          }) cfg.targets.nodes;
          scrape_interval = "15s";
          relabel_configs = [
            {
              source_labels = [ "__address__" ];
              target_label = "instance";
              regex = "([^:]+):.+";
              replacement = "\${1}";
            }
          ];
        }
        # TODO: generate/discover
        {
          job_name = "unifi-poller";
          static_configs = [
            {
              targets = [ "localhost:${toString cfg.ports.unifiPoller}" ];
            }
          ];
        }
      ]
      ++ lib.optionals (cfg.targets ? hass && cfg.targets.hass != []) [
        {
          job_name = "home-assistant";
          static_configs = map (target: {
            targets = [ "${target.ip}:${toString target.home_assistant_port}" ];
            labels = {
              hostname = target.hostname;
            };
          }) cfg.targets.hass;
          metrics_path = "/api/prometheus";
          scrape_interval = "60s";
          authorization = {
            credentials_file = cfg.secretPaths.hass_prometheus_token;
          };
        }
        {
          job_name = "mqtt-exporter";
          static_configs = map (target: {
            targets = [ "${target.ip}:${toString target.mqtt_exporter_port}" ];
            labels = {
              hostname = target.hostname;
            };
          }) cfg.targets.hass;
          scrape_interval = "30s";
        }
      ]
      ++ lib.optional config.services.prometheus.alertmanager.enable [
        {
          job_name = "alertmanager";
          static_configs = [
            {
              targets = [ "localhost:${toString cfg.ports.alertmanager}" ];
            }
          ];
        }
      ];
    };

  services.nginx.virtualHosts."${cfg.fqdn}".locations."/prometheus/" = lib.mkIf cfg.enable {
    proxyPass = "http://localhost:${toString cfg.ports.prometheus}/";
  };
}
