{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.monitoring;

  lokiRulesDir = pkgs.runCommand "loki-rules" { } ''
    mkdir -p $out/fake
    cat > $out/fake/paperless.yaml << 'EOF'
    groups:
      - name: paperless
        rules:
          - alert: PaperlessConsumeError
            expr: 'count_over_time({app="celery", host="calliope"} |~ "ERROR" [15m]) > 0'
            for: 0s
            labels:
              severity: warning
            annotations:
              summary: "Paperless consume error on calliope"
              description: "Paperless consumer logged errors in the last 15 minutes."
    EOF
  '';
in
{
  services.loki = {

    configuration = {
      auth_enabled = false;

      analytics = {
        reporting_enabled = false;
      };

      server = {
        http_listen_port = cfg.ports.loki;
        http_listen_address = "localhost";
        grpc_server_max_recv_msg_size = 52428800;
        grpc_server_max_send_msg_size = 52428800;
      };

      query_scheduler = {
        grpc_client_config = {
          max_recv_msg_size = 52428800;
          max_send_msg_size = 52428800;
        };
      };

      frontend_worker = {
        grpc_client_config = {
          max_recv_msg_size = 52428800;
          max_send_msg_size = 52428800;
        };
      };

      ingester_client = {
        grpc_client_config = {
          max_recv_msg_size = 52428800;
          max_send_msg_size = 52428800;
        };
      };

      limits_config = {
        max_entries_limit_per_query = 1000000;
        max_query_length = "12000h";
        reject_old_samples = true;
        reject_old_samples_max_age = "168h";
        allow_structured_metadata = true;
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

      schema_config = {
        configs = [
          {
            from = "2024-01-01";
            store = "tsdb";
            object_store = "filesystem";
            schema = "v13";
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

      compactor = {
        working_directory = "/var/lib/loki/compactor";
        compaction_interval = "5m";
      };

      ruler = {
        storage = {
          type = "local";
          local = {
            directory = "${lokiRulesDir}";
          };
        };
        rule_path = "/var/lib/loki/rules-temp";
        alertmanager_url = "http://localhost:${toString cfg.ports.alertmanager}";
        ring = {
          kvstore = {
            store = "inmemory";
          };
        };
        enable_api = true;
      };
    };
  };
}
