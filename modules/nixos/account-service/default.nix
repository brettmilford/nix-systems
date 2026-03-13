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
  api-server-pkg = flake.packages.${pkgs.system}.api-server;
  webhook-server-pkg = flake.packages.${pkgs.system}.webhook-server;
  categoriser-pkg = flake.packages.${pkgs.system}.categoriser;
  webhook-server-cfg = pkgs.writeTextDir "config.toml" (builtins.readFile ./webhook-config.toml);
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
      description = "Port the api-server listens on";
    };
    webhookPort = mkOption {
      type = types.port;
      default = 8082;
      description = "Port the webhook-server listens on";
    };
    categoriserPort = mkOption {
      type = types.port;
      default = 8083;
      description = "Port the categoriser listens on";
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

    systemd.services.account-api-server = {
      description = "Account Service API Server";
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
          "CATEGORISER_URL=http://127.0.0.1:${toString cfg.categoriserPort}"
        ];
        WorkingDirectory = "${api-server-pkg}/share/api-server";
        ExecStart = "${api-server-pkg}/bin/api-server --web.listen-address 127.0.0.1:${toString cfg.port} --web.external-url https://${cfg.fqdn}";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    systemd.services.account-webhook-server = {
      description = "Account Service Webhook Server";
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
        ExecStart = "${webhook-server-pkg}/bin/webhook-server --verbose --web.listen-address 127.0.0.1:${toString cfg.webhookPort} --web.external-url https://${cfg.fqdn} --config ${webhook-server-cfg}/config.toml";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    systemd.services.account-categoriser-server = {
      description = "Account Service Categoriser";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "postgresql.service"
        "account-api-server.service"
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
        ExecStart = "${categoriser-pkg}/bin/categoriser --verbose --web.listen-address 127.0.0.1:${toString cfg.categoriserPort} --config ${webhook-server-cfg}/config.toml";
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
        proxyPass = "http://127.0.0.1:${toString cfg.webhookPort}";
      };
    };
  };
}
