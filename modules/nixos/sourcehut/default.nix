{
  self,
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.srht;

  serviceDefs = {
    meta = {
      ip = "10.0.100.10";
      webPort = 5000;
      apiPort = 5100;
    };
    git = {
      ip = "10.0.100.11";
      webPort = 5001;
      apiPort = 5101;
    };
    todo = {
      ip = "10.0.100.12";
      webPort = 5002;
      apiPort = 5102;
    };
    hub = {
      ip = "10.0.100.13";
      webPort = 5003;
      apiPort = 5103;
    };
    man = {
      ip = "10.0.100.14";
      webPort = 5004;
      apiPort = 5104;
    };
    pages = {
      ip = "10.0.100.15";
      webPort = 5005;
      apiPort = 5105;
    };
    builds = {
      ip = "10.0.100.16";
      webPort = 5006;
      apiPort = 5106;
    };
  };

  enabledServices = lib.filterAttrs (name: _: builtins.elem name cfg.services) serviceDefs;
in
{
  imports = [
    ../incus.nix
    ../gateway.nix
    ./containers.nix
  ];

  options.services.srht = {
    enable = mkEnableOption "SourceHut";

    domain = mkOption {
      type = types.str;
      default = "cirriform.au";
      description = "Base domain for SourceHut services";
    };

    services = mkOption {
      type = types.listOf (
        types.enum [
          "meta"
          "git"
          "todo"
          "hub"
          "man"
          "pages"
          "builds"
        ]
      );
      default = [
        "meta"
        "git"
        "todo"
        "hub"
        "man"
        "pages"
        "builds"
      ];
      description = "Which SourceHut services to enable";
    };

    dataPath = mkOption {
      type = types.str;
      default = "/srv/data/sourcehut";
      description = "Base data directory for SourceHut";
    };

    secretPaths = mkOption {
      type = types.attrs;
      description = "Agenix secret file paths (auto-populated by framework)";
    };

    serviceDefs = mkOption {
      type = types.attrs;
      internal = true;
      default = enabledServices;
      description = "Resolved service definitions for enabled services";
    };
  };

  config = mkIf cfg.enable {

    services.incus = {
      enable = true;
      dataPath = "${cfg.dataPath}/containers";
    };

    services.gateway.enable = true;

    services.postgresql = {
      enable = true;
      enableTCPIP = true;
      authentication = ''
        host all srht 10.0.100.0/24 scram-sha-256
      '';
      ensureUsers = [
        { name = "srht"; }
      ];
    };

    systemd.services.srht-db-setup = {
      description = "Create SourceHut databases and set password";
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];
      wantedBy = [ "multi-user.target" ];
      path = [ config.services.postgresql.package ];
      script = ''
        source ${cfg.secretPaths."srht-secrets-env"}

        psql -c "ALTER ROLE srht WITH PASSWORD '$SRHT_DB_PASSWORD';"

        ${lib.concatMapStringsSep "\n" (name: ''
          psql -tc "SELECT 1 FROM pg_database WHERE datname = '${name}_srht'" | grep -q 1 || \
            psql -c "CREATE DATABASE ${name}_srht OWNER srht;"
        '') cfg.services}
      '';
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };

    services.redis.servers.sourcehut = {
      enable = true;
      bind = "10.0.100.1";
      port = 6379;
    };

    services.nginx.virtualHosts = lib.mapAttrs' (
      name: svc:
      lib.nameValuePair "${name}.${cfg.domain}" {
        locations."/" = {
          proxyPass = "http://${svc.ip}:${toString svc.webPort}";
        };
        locations."/query" = {
          proxyPass = "http://${svc.ip}:${toString svc.apiPort}";
        };
      }
    ) enabledServices;

    systemd.tmpfiles.rules = [
      "d ${cfg.dataPath} 0755 root root -"
      "d ${cfg.dataPath}/containers 0755 root root -"
    ];
  };
}
