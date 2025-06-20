# NixOS Monitoring Stack Module
# Deploys Prometheus, Grafana, Loki, and Alertmanager
#
# Based on verified patterns from NixOS community examples
# Please verify specific syntax for your NixOS version/channel
# Last researched: June 2025 - some syntax may need verification

{ config, lib, pkgs, ... }:

let
  cfg = config.services.lma;

  # Monitoring configuration
  domain = "lma.local";  # Adjust to your domain

  # Network settings - adjust to your environment
  listenLocal = "127.0.0.1";
  listenNetwork = "0.0.0.0";

  # Ports - keeping services on localhost, exposed via nginx
  prometheusPort = 9090;
  grafanaPort = 3000;
  lokiPort = 3100;
  alertmanagerPort = 9093;
  promtailPort = 9080;
in
{
  options.services.lma = {
    enable = lib.mkEnableOption "monitoring stack with Prometheus, Grafana, Loki, and Alertmanager";

    domain = lib.mkOption {
      type = lib.types.str;
      default = "lma.internal";
      description = "Domain name for the monitoring services";
    };
  };

  config = lib.mkIf cfg.enable {

    # Prometheus configuration
    services.prometheus = {
      enable = true;
      port = prometheusPort;
      listenAddress = listenNetwork;

      # Basic global configuration
      globalConfig = {
        scrape_interval = "15s";
        evaluation_interval = "15s";
      };

      # Alert rules - basic system monitoring
      rules = [
        (builtins.toJSON {
          groups = [{
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
          }];
        })
      ];

      # Scrape configurations
      scrapeConfigs = [
        # Needs to be generated from actually running exporter configs
        # {
        #   job_name = "prometheus";
        #   static_configs = [{
        #     targets = [ "${listenAddress}:${toString prometheusPort}" ];
        #   }];
        # }
        {
          job_name = "node-exporter";
          static_configs = [{
            targets = [ "${listenLocal}:${toString config.services.prometheus.exporters.node.port}" ];
          }];
        }
        # {
        #   job_name = "loki";
        #   static_configs = [{
        #     targets = [ "${listenAddress}:${toString lokiPort}" ];
        #   }];
        # }
        # {
        #   job_name = "grafana";
        #   static_configs = [{
        #     targets = [ "${listenAddress}:${toString grafanaPort}" ];
        #   }];
        # }
        # {
        #   job_name = "alertmanager";
        #   static_configs = [{
        #     targets = [ "${listenAddress}:${toString alertmanagerPort}" ];
        #   }];
        # }
      ];

      # Connect to Alertmanager
      alertmanagers = [{
        static_configs = [{
          targets = [ "${listenLocal}:${toString alertmanagerPort}" ];
        }];
      }];

      # Enable node exporter
      exporters = {
        node = {
          enable = true;
          port = 9100;
          enabledCollectors = [
            "systemd"
            "filesystem"
            "meminfo"
            "loadavg"
            "stat"
          ];
        };
      };
    };

    # Alertmanager configuration
    services.prometheus.alertmanager = {
      enable = true;
      port = alertmanagerPort;
      listenAddress = listenLocal;

      # Basic alertmanager configuration
      # Please verify this syntax for your NixOS version
      configuration = {
        global = {
          smtp_smarthost = "localhost:587";
          smtp_from = "alerts@${cfg.domain}";
        };

        route = {
          group_by = [ "alertname" ];
          group_wait = "10s";
          group_interval = "10s";
          repeat_interval = "1h";
          receiver = "web.hook";
        };

        receivers = [
          {
            name = "web.hook";
            # Add your notification methods here
            # webhook_configs = [{
            #   url = "http://example.com/webhook";
            # }];
            # email_configs = [{
            #   to = "admin@example.com";
            #   subject = "Alert: {{ .GroupLabels.alertname }}";
            #   body = "{{ range .Alerts }}{{ .Annotations.description }}{{ end }}";
            # }];
          }
        ];
      };
    };

    # Loki configuration with modern TSDB setup
    services.loki = {
      enable = true;

      # Note: Please verify this configuration structure for your NixOS version
      # The structure may vary between nixpkgs versions
      configuration = {
        auth_enabled = false;

        server = {
          http_listen_port = lokiPort;
          http_listen_address = listenNetwork;
        };

        common = {
          ring = {
            instance_addr = "127.0.0.1";
            kvstore = {
              store = "inmemory";
            };
          };
          replication_factor = 1;
          path_prefix = "/var/lib/loki";
        };

        # Modern schema configuration with TSDB
        # Based on current Loki documentation
        schema_config = {
          configs = [
            {
              from = "2024-01-01";
              store = "tsdb";
              object_store = "filesystem";
              schema = "v13";  # Latest recommended schema version
              index = {
                prefix = "index_";
                period = "24h";
              };
            }
          ];
        };

        storage_config = {
          tsdb_shipper = {
            active_index_directory = "/var/lib/loki/tsdb-index";
            cache_location = "/var/lib/loki/tsdb-cache";
          };
          filesystem = {
            directory = "/var/lib/loki/chunks";
          };
        };

        limits_config = {
          reject_old_samples = true;
          reject_old_samples_max_age = "168h";
          allow_structured_metadata = true;  # Required for Loki 3.0+
        };

        compactor = {
          working_directory = "/var/lib/loki/compactor";
          compaction_interval = "5m";
        };
      };
    };

    # Promtail configuration for log shipping
    services.promtail = {
      enable = true;

      configuration = {
        server = {
          http_listen_port = promtailPort;
          http_listen_address = listenLocal;
        };

        clients = [{
          url = "http://${listenLocal}:${toString lokiPort}/loki/api/v1/push";
        }];

        scrape_configs = [
          {
            job_name = "systemd-journal";
            journal = {
              max_age = "12h";
              labels = {
                job = "systemd-journal";
                host = config.networking.hostName;
              };
            };
            relabel_configs = [
              {
                source_labels = [ "__journal__systemd_unit" ];
                target_label = "unit";
              }
              {
                source_labels = [ "__journal_priority" ];
                target_label = "priority";
              }
            ];
          }
          # Add more log sources as needed
          # {
          #   job_name = "nginx-logs";
          #   static_configs = [{
          #     targets = [ "localhost" ];
          #     labels = {
          #       job = "nginx";
          #       __path__ = "/var/log/nginx/*.log";
          #     };
          #   }];
          # }
        ];
      };
    };

    age.secrets.grafana_pass = {
      file = ../../../secrets/grafana_pass.age;
      owner = "grafana";
      group = "grafana ";
    };

    age.secrets.grafana_key = {
      file = ../../../secrets/grafana_key.age;
      owner = "grafana";
      group = "grafana ";
    };


    # Grafana configuration
    services.grafana = {
      enable = true;
      settings = {
        server = {
          http_addr = listenLocal;  # Use IP address, not hostname (Grafana 11.3+ requirement)
          http_port = grafanaPort;
          domain = cfg.domain;
          root_url = "http://${cfg.domain}/";
        };

        # Security settings
        security = {
          admin_user = "admin";
          admin_password = "$__file{${config.age.secrets.grafana_pass.path}}";
          secret_key = "$__file{${config.age.secrets.grafana_key.path}}";
        };

        # Analytics
        analytics = {
          reporting_enabled = false;
        };
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
              url = "http://${listenLocal}:${toString prometheusPort}";
              isDefault = true;
            }
            {
              name = "Loki";
              type = "loki";
              access = "proxy";
              url = "http://${listenLocal}:${toString lokiPort}";
            }
          ];
        };

        # You can add dashboard provisioning here
        # dashboards.settings = {
        #   apiVersion = 1;
        #   providers = [{
        #     name = "default";
        #     folder = "";
        #     type = "file";
        #     options.path = "/etc/grafana/dashboards";
        #   }];
        # };
      };
    };

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;

      virtualHosts = {
        "${cfg.domain}" = {
          enableACME = true;
          forceSSL = true;
          locations = {
            "/" = {
              proxyPass = "http://${listenLocal}:${toString grafanaPort}/";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
              '';
            };
            "/prometheus/" = {
              proxyPass = "http://${listenLocal}:${toString prometheusPort}/";
              extraConfig = ''
                proxy_set_header Host $host;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              '';
            };
            "/loki/" = {
              proxyPass = "http://${listenLocal}:${toString lokiPort}/";
            };
            "/alertmanager/" = {
              proxyPass = "http://${listenLocal}:${toString alertmanagerPort}/";
            };
          };
        };
      };
    };

    # Firewall configuration
    networking.firewall = {
      allowedTCPPorts = [
        prometheusPort
        lokiPort
      ];
    };

    # System packages for management tools
    environment.systemPackages = with pkgs; [
      prometheus-alertmanager  # Provides amtool for testing
    ];

    # Ensure data directories exist with proper permissions
    systemd.tmpfiles.rules = [
      "d /var/lib/loki 0755 loki loki -"
      "d /var/lib/loki/chunks 0755 loki loki -"
      "d /var/lib/loki/tsdb-index 0755 loki loki -"
      "d /var/lib/loki/tsdb-cache 0755 loki loki -"
      "d /var/lib/loki/compactor 0755 loki loki -"
    ];
  };
}

# Usage Example:
# In your configuration.nix or as a separate module:
#
# {
#   imports = [ ./lma.nix ];
#
#   services.lma = {
#     enable = true;
#     domain = "monitoring.example.com";
#     externalAccess = true;
#     openFirewall = true;
#   };
# }
#
# Access URLs (when externalAccess = true):
# - Grafana: https://monitoring.example.com/
# - Prometheus: https://monitoring.example.com/prometheus/
# - Loki: https://monitoring.example.com/loki/
# - Alertmanager: https://monitoring.example.com/alertmanager/
#
# Default credentials:
# - Grafana: admin/admin (change immediately in production!)
#
# IMPORTANT NOTES:
# 1. Change default passwords and secrets in production
# 2. Verify Loki configuration syntax for your NixOS version
# 3. Configure proper notification channels in Alertmanager
# 4. Add TLS/SSL for production deployments
# 5. Consider using NixOS secrets management for sensitive data
# 6. Test schema migrations carefully in non-production first
