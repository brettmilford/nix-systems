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

  incusServiceDefs = {
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

  # meta and git are the always-on core; everything else is opt-in via cfg.services.
  enabledServiceNames = lib.unique (
    [
      "meta"
      "git"
    ]
    ++ cfg.services
  );
  enabledServices = lib.filterAttrs (
    name: _: builtins.elem name enabledServiceNames
  ) incusServiceDefs;
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
          "todo"
          "hub"
          "man"
          "pages"
          "builds"
        ]
      );
      default = [ ];
      example = [ "builds" ];
      description = ''
        Optional SourceHut services to enable in addition to the always-on
        core (meta and git).
      '';
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
      dataPath = "${cfg.dataPath}/incus";
    };

    services.gateway.enable = true;

    # git-over-SSH: the srht-git container's sshd is exposed on this host port
    # via an incus proxy device (see containers.nix).
    networking.firewall.allowedTCPPorts = [ 2222 ];

    # SourceHut containers relay outbound mail through the host's postfix on the
    # incus bridge, so it must listen there and trust the container subnet.
    services.mail-relay.listenInterfaces = [
      "127.0.0.1"
      "10.0.100.1"
    ];
    services.mail-relay.mynetworks = [
      "127.0.0.0/8"
      "[::1]/128"
      "10.0.100.0/24"
    ];

    systemd.services.postfix = {
      after = [ "incus.service" ];
      requires = [ "incus.service" ];
    };

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
      restartTriggers = [ config.age.secrets."srht-secrets-env".file ];
      after = [
        "postgresql.service"
        "postgresql-setup.service"
      ];
      requires = [
        "postgresql.service"
        "postgresql-setup.service"
      ];
      wantedBy = [ "multi-user.target" ];
      path = [ config.services.postgresql.package ];
      script = ''
        set -f
        source "$CREDENTIALS_DIRECTORY/srht-env"
        set +f

        psql -U postgres -c "ALTER ROLE srht WITH PASSWORD '$SRHT_DB_PASSWORD';"

        ${lib.concatMapStringsSep "\n" (name: ''
          psql -U postgres -tc "SELECT 1 FROM pg_database WHERE datname = '${name}_srht'" | grep -q 1 || \
            psql -U postgres -c "CREATE DATABASE ${name}_srht OWNER srht;"
        '') enabledServiceNames}
      '';
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "postgres";
        LoadCredential = [ "srht-env:${cfg.secretPaths."srht-secrets-env"}" ];
      };
    };

    services.redis.servers.sourcehut = {
      enable = true;
      bind = "10.0.100.1";
      port = 6379;
    };

    systemd.services.redis-sourcehut = {
      after = [ "incus-preseed.service" ];
      requires = [ "incus-preseed.service" ];
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
    ];
  };
}
