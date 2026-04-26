{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.garmin-collect;
  flake = builtins.getFlake "git+ssh://git@calliope/~/garmin-collect.git?rev=${cfg.rev}";
  garmin-collect-pkg = flake.packages.${pkgs.system}.default;
  dashboardsDir = pkgs.runCommand "garmin-dashboards" { } ''
    mkdir -p $out
    cp ${flake}/infra/grafana/provisioning/dashboards/json/*.json $out/
  '';
in
{
  imports = [
    ../gateway.nix
  ];

  options.services.garmin-collect = {
    enable = mkEnableOption "garmin-collect";
    rev = mkOption {
      type = types.str;
      description = "Pinned git revision of garmin-collect flake";
    };
    fqdn = mkOption {
      type = types.str;
      description = "FQDN for the manual health entry nginx virtualHost";
    };
    measurePort = mkOption {
      type = types.port;
      default = 8642;
      description = "Port the manual-health-serve listens on";
    };
    secretPaths = mkOption {
      type = types.attrs;
      default = { };
      description = "Secret file paths provided by the service module framework";
    };
  };

  config = mkIf cfg.enable {

    # PostgreSQL database, garmin user (owner), grafana user (read access)
    services.postgresql = {
      ensureDatabases = [ "garmin" ];
      ensureUsers = [
        {
          name = "garmin";
          ensureDBOwnership = true;
        }
        {
          name = "grafana";
          ensureClauses.login = true;
        }
      ];
    };

    # Grant grafana read access to garmin database after schema is created
    systemd.services.garmin-collect-grant = {
      description = "Grant Grafana read access to garmin database";
      after = [ "postgresql.service" "garmin-collect.service" ];
      wants = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "garmin";
        ExecStart = pkgs.writeShellScript "garmin-grant" ''
          ${config.services.postgresql.package}/bin/psql -d garmin -c "
            GRANT CONNECT ON DATABASE garmin TO grafana;
            GRANT USAGE ON SCHEMA public TO grafana;
            GRANT SELECT ON ALL TABLES IN SCHEMA public TO grafana;
            ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO grafana;
          "
        '';
      };
    };

    # Systemd oneshot service
    systemd.services.garmin-collect = {
      description = "Garmin Connect data collector";
      after = [
        "network.target"
        "postgresql.service"
      ];
      wants = [ "postgresql.service" ];
      serviceConfig = {
        User = "garmin";
        Group = "garmin";
        Type = "oneshot";
        StateDirectory = "garmin-collect";
        EnvironmentFile = cfg.secretPaths."garmin-collect.env";
        Environment = [
          "DATABASE_URL=postgresql:///garmin?host=/run/postgresql"
          "GARMIN_TOKEN_DIR=/var/lib/garmin-collect"
        ];
        ExecStart = "${garmin-collect-pkg}/bin/garmin-collect";
      };
    };

    # Systemd timer — every 30 minutes
    systemd.timers.garmin-collect = {
      description = "Garmin Connect data collection timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*:0/30";
        Persistent = true;
        RandomizedDelaySec = "5m";
        Unit = "garmin-collect.service";
      };
    };

    # System user and group
    users.users.garmin = {
      isSystemUser = true;
      group = "garmin";
    };
    users.groups.garmin = { };

    # Grafana PostgreSQL datasource
    services.grafana.provision.datasources.settings.datasources = [
      {
        name = "Garmin";
        uid = "garmin-postgres";
        type = "grafana-postgresql-datasource";
        access = "proxy";
        url = "/run/postgresql";
        database = "garmin";
        user = "grafana";
        isDefault = false;
        jsonData = {
          sslmode = "disable";
          postgresVersion = 1700;
        };
      }
    ];

    # Grafana dashboard provider
    services.grafana.provision.dashboards.settings.providers = [
      {
        name = "garmin";
        folder = "Garmin";
        type = "file";
        disableDeletion = false;
        editable = true;
        options.path = "${dashboardsDir}";
      }
    ];

    # Manual health data entry web service
    systemd.services.garmin-collect-web = {
      description = "Manual Health Data Entry";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
      ];
      wants = [ "postgresql.service" ];
      serviceConfig = {
        User = "garmin";
        Group = "garmin";
        EnvironmentFile = cfg.secretPaths."garmin-collect.env";
        Environment = [
          "DATABASE_URL=postgresql:///garmin?host=/run/postgresql"
          "MANUAL_HEALTH_PORT=${toString cfg.measurePort}"
        ];
        ExecStart = "${garmin-collect-pkg}/bin/manual-health-serve";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    services.gateway.enable = true;
    services.nginx.virtualHosts.${cfg.fqdn} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.measurePort}";
      };
    };
  };
}
