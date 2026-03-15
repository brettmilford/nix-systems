{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.account-service;
  flake = builtins.getFlake "git+ssh://git@calliope/~/account-service.git?rev=${cfg.rev}";
  api-service-pkg = flake.packages.${pkgs.system}.api-service;
  parser-service-pkg = flake.packages.${pkgs.system}.parser-service;
  labeler-service-pkg = flake.packages.${pkgs.system}.labeler-service;
  parser-service-cfg = pkgs.writeTextDir "config.toml" (builtins.readFile ./parser-config.toml);
in
{
  imports = [
    ../gateway.nix
  ];

  options.services.account-service = {
    enable = mkEnableOption "account-service";
    fqdn = mkOption {
      type = types.str;
      description = "FQDN for the account service nginx virtualHost";
    };
    rev = mkOption {
      type = types.str;
      description = "Pinned git revision of account-service flake";
    };
    port = mkOption {
      type = types.port;
      default = 8080;
      description = "Port the api-service listens on";
    };
    parserPort = mkOption {
      type = types.port;
      default = 8081;
      description = "Port the parser-service listens on";
    };
    labelerPort = mkOption {
      type = types.port;
      default = 8082;
      description = "Port the labeler-service listens on";
    };
    secretPaths = mkOption {
      type = types.attrs;
      default = { };
      description = "Secret file paths provided by the service module framework";
    };
  };

  config = mkIf cfg.enable {

    services.postgresql = {
      ensureDatabases = [ "accsvc" ];
      ensureUsers = [
        {
          name = "accsvc";
          ensureDBOwnership = true;
        }
      ];
    };

    systemd.services.account-api = {
      description = "Account Service API";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
      ];
      wants = [ "postgresql.service" ];
      serviceConfig = {
        User = "accsvc";
        Group = "accsvc";
        EnvironmentFile = cfg.secretPaths."account-service.env";
        Environment = [
          "DB_HOST=/run/postgresql"
          "DB_USER=accsvc"
          "DB_NAME=accsvc"
          "DB_SSLMODE=disable"
          "LABELER_URL=http://127.0.0.1:${toString cfg.labelerPort}"
          "PAPERLESS_URL=https://paperless.cirriform.au"
        ];
        WorkingDirectory = "${api-service-pkg}/share/api-service";
        ExecStart = "${api-service-pkg}/bin/api-service --web.listen-address 127.0.0.1:${toString cfg.port} --web.external-url https://${cfg.fqdn}";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    systemd.services.account-parser = {
      description = "Account Service Parser";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
      ];
      wants = [ "postgresql.service" ];
      serviceConfig = {
        User = "accsvc";
        Group = "accsvc";
        EnvironmentFile = cfg.secretPaths."account-service.env";
        Environment = [
          "DB_HOST=/run/postgresql"
          "DB_USER=accsvc"
          "DB_NAME=accsvc"
          "DB_SSLMODE=disable"
        ];
        ExecStart = "${parser-service-pkg}/bin/parser-service --verbose --web.listen-address 127.0.0.1:${toString cfg.parserPort} --web.external-url https://${cfg.fqdn} --config ${parser-service-cfg}/config.toml";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    systemd.services.account-labeler = {
      description = "Account Service Labeler";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
        "account-api.service"
      ];
      wants = [ "postgresql.service" ];
      serviceConfig = {
        User = "accsvc";
        Group = "accsvc";
        EnvironmentFile = cfg.secretPaths."account-service.env";
        Environment = [
          "DB_HOST=/run/postgresql"
          "DB_USER=accsvc"
          "DB_NAME=accsvc"
          "DB_SSLMODE=disable"
        ];
        ExecStart = "${labeler-service-pkg}/bin/labeler-service --verbose --web.listen-address 127.0.0.1:${toString cfg.labelerPort} --config ${parser-service-cfg}/config.toml";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    users.users.accsvc = {
      isSystemUser = true;
      group = "accsvc";
    };
    users.groups.accsvc = { };

    services.gateway.enable = true;
    services.nginx.virtualHosts.${cfg.fqdn} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.port}";
        proxyWebsockets = true;
      };
      locations."/webhook/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.parserPort}";
      };
    };
  };
}
