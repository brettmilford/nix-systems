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
  services.loki = {

    configuration = {
      auth_enabled = false;

      analytics = {
        reporting_enabled = false;
      };

      server = {
        http_listen_port = cfg.ports.loki;
        http_listen_address = "localhost";
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
            schema = "v13"; # Latest recommended schema version
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
        allow_structured_metadata = true; # Required for Loki 3.0+
      };

      compactor = {
        working_directory = "/var/lib/loki/compactor";
        compaction_interval = "5m";
      };
    };
  };
  # Ensure data directories exist with proper permissions
  #systemd.tmpfiles.rules = [
  #  "d /var/lib/loki 0755 loki loki -"
  #  "d /var/lib/loki/chunks 0755 loki loki -"
  #  "d /var/lib/loki/tsdb-index 0755 loki loki -"
  #  "d /var/lib/loki/tsdb-cache 0755 loki loki -"
  #  "d /var/lib/loki/compactor 0755 loki loki -"
  #];
}
