{ self, config, lib, pkgs, ... }:

let
  cfg = config.services.monitoring;

  # Network settings - adjust to your environment
  listenLocal = "127.0.0.1";
  listenNetwork = "0.0.0.0";

  # Ports - keeping services on localhost, exposed via nginx
  prometheusPort = 9090;
  prometheusNodeExporterPort = 9100;
  prometheusSNMPExporterPort = 9116;
  grafanaPort = 3000;
  lokiPort = 3100;
  alertmanagerPort = 9093;
  unifiPollerPort = 9130;

in
{
  imports = [
    ./fluentd
  ];

  options.services.monitoring = {
    enable = lib.mkEnableOption "monitoring stack with Prometheus, Grafana, Loki, and Alertmanager";

    domain = lib.mkOption {
      type = lib.types.str;
      default = "monitoring.internal";
      description = "Domain name for the monitoring services";
    };

    enableAlertManager = lib.mkEnableOption "Add Alert manager";
    enableUnpoller = lib.mkEnableOption "Enable Unpoller";
  };

  config = lib.mkIf cfg.enable {

    age.secrets.snmp_env = {
      file = "${self}/secrets/snmp.env.age";
    };

    # Prometheus configuration
    services.prometheus = {
      enable = true;
      port = prometheusPort;
      listenAddress = listenLocal;
      webExternalUrl = "http://${cfg.domain}/prometheus";
      extraFlags = ["--web.route-prefix=/"];

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
        {
          job_name = "prometheus";
          static_configs = [{
            targets = [ "${listenLocal}:${toString prometheusPort}" ];
          }];
        }
        {
          job_name = "loki";
          static_configs = [{
            targets = [ "${listenLocal}:${toString lokiPort}" ];
          }];
        }
        {
          job_name = "grafana";
          static_configs = [{
            targets = [ "${listenLocal}:${toString grafanaPort}" ];
          }];
        }
        {
          job_name = "node-exporter";
          static_configs = [{
            targets = [ "${listenLocal}:${toString prometheusNodeExporterPort}"
                        "calliope.zt:${toString prometheusNodeExporterPort}"
                      ];
          }];
        }
        {
          job_name = "unifi-poller";
          static_configs = [{
            targets = ["${listenLocal}:${toString unifiPollerPort}"];
          }];
        }
        {
          job_name = "snmp-exporter";
          static_configs = [{
            targets = [
              "opnsense"
            ];
          }];
          metrics_path = "/snmp";
          params = {
            module = ["opnsense"];
            auth = ["public_v3"];
          };
          relabel_configs = [
            {
              source_labels = ["__address__"];
              target_label = "__param_target";
            }
            {
              source_labels = ["__param_target"];
              target_label = "instance";
            }
            {
              target_label = "__address__";
              replacement = "${listenLocal}:${toString prometheusSNMPExporterPort}";
            }
          ];
        }
      ] ++ lib.optional config.services.prometheus.alertmanager.enable [
        {
          job_name = "alertmanager";
          static_configs = [{
            targets = [ "${listenLocal}:${toString alertmanagerPort}" ];
          }];
        }
      ];

      # Enable node exporter
      exporters = {
        snmp = {
          enable = true;
          configurationPath = ./snmp.yml;
          environmentFile = "${config.age.secrets.snmp_env.path}";
        };

        node = {
          enable = true;
          port = prometheusNodeExporterPort;
          enabledCollectors = [
            "systemd"
            "filesystem"
            "meminfo"
            "loadavg"
            "stat"
            "processes"
            "interrupts"
          ];
        };
      };
    };

    # Alertmanager configuration
    services.prometheus.alertmanager = {
      enable = cfg.enableAlertManager;
      port = alertmanagerPort;
      listenAddress = listenLocal;
      webExternalUrl = "http://${cfg.domain}/alertmanager/";
      extraFlags = ["--web.route-prefix=/"];

      # TODO: verify
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

      configuration = {
        auth_enabled = false;

        analytics = {
          reporting_enabled = false;
        };

        server = {
          http_listen_port = lokiPort;
          http_listen_address = listenLocal;
        # Server-side gRPC limits
          grpc_server_max_recv_msg_size = 52428800; # 50MB
          grpc_server_max_send_msg_size = 52428800; # 50MB
        };

        # Query scheduler gRPC client config
        query_scheduler = {
          grpc_client_config = {
            max_recv_msg_size = 52428800; # 50MB
            max_send_msg_size = 52428800; # 50MB
          };
        };

        # Frontend worker gRPC client config
        frontend_worker = {
          grpc_client_config = {
            max_recv_msg_size = 52428800; # 50MB
            max_send_msg_size = 52428800; # 50MB
          };
        };

        # Ingester client gRPC config
        ingester_client = {
          grpc_client_config = {
            max_recv_msg_size = 52428800; # 50MB
            max_send_msg_size = 52428800; # 50MB
          };
        };

        # Other recommended settings for large log handling
        limits_config = {
          max_entries_limit_per_query = 1000000;
          max_query_length = "12000h"; # Allow longer time ranges
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

    age.secrets.grafana_pass = {
      file = "${self}/secrets/grafana_pass.age";
      owner = "grafana";
      group = "grafana ";
    };

    age.secrets.grafana_key = {
      file = "${self}/secrets/grafana_key.age";
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
          root_url = "https://${cfg.domain}/";
        };

        # Security settings
        security = {
          admin_user = "admin";
          admin_password = "$__file{${config.age.secrets.grafana_pass.path}}";
          secret_key = "$__file{${config.age.secrets.grafana_key.path}}";
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

        dashboards.settings = {
          apiVersion = 1;
          providers = [{
            name = "default";
            folder = "";
            type = "file";
            options.path = "/var/lib/grafana/dashboards";
          }];
        };
      };
    };

    age.secrets.unifipoller_pass = {
      file = "${self}/secrets/unifipoller_pass.age";
      owner = "unifi-poller";
    };

    services.unpoller = {
      enable = cfg.enableUnpoller;
      influxdb.disable = true;
      unifi.defaults.verify_ssl = false;
      unifi.defaults.user = "unifipoller";
      unifi.defaults.pass = config.age.secrets.unifipoller_pass.path;
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
            };
            "/prometheus/" = {
              proxyPass = "http://${listenLocal}:${toString prometheusPort}/";
            };
            "/loki/" = {
              proxyPass = "http://${listenLocal}:${toString lokiPort}/";
            };
            "/alertmanager/" = lib.mkIf cfg.enableAlertManager {
              proxyPass = "http://${listenLocal}:${toString alertmanagerPort}/";
            };
          };
        };
      };
    };

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
