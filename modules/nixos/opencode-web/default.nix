{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.opencode-web;
  pythonEnv = pkgs.python3.withPackages (ps: [ ps.pyyaml ]);
in
{
  imports = [
    ../gateway.nix
  ];

  options.services.opencode-web = {
    enable = mkEnableOption "opencode-web";

    fqdn = mkOption {
      type = types.str;
      description = "FQDN for the opencode-web nginx virtualHost";
    };

    port = mkOption {
      type = types.port;
      default = 4096;
      description = "Port the opencode web server listens on";
    };

    dataPath = mkOption {
      type = types.str;
      description = "Directory where opencode-web data and projects are stored";
    };
  };

  config = mkIf cfg.enable {
    users.users.opencode = {
      isNormalUser = true;
      group = "opencode";
      home = cfg.dataPath;
      shell = pkgs.bashInteractive;
    };
    users.groups.opencode = { };

    systemd.tmpfiles.settings."10-opencode-web" = {
      ${cfg.dataPath}.d = {
        mode = "0755";
        user = "opencode";
        group = "opencode";
      };
    };

    systemd.services.opencode-web = {
      description = "OpenCode Web Interface";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        User = "opencode";
        Group = "opencode";
        WorkingDirectory = cfg.dataPath;
        Environment = [
          "HOME=${cfg.dataPath}"
          "SHELL=${pkgs.bashInteractive}/bin/bash"
          "PATH=${pkgs.bashInteractive}/bin:${pkgs.git}/bin:${pkgs.coreutils}/bin:${pythonEnv}/bin:/run/wrappers/bin:/usr/bin:/bin"
          "OPENCODE_ENABLE_EXA=1"
        ];
        ExecStart = "${pkgs.opencode}/bin/opencode web --port ${toString cfg.port} --hostname 127.0.0.1 --cors https://${cfg.fqdn}";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    services.gateway.enable = true;
    services.nginx.virtualHosts.${cfg.fqdn} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString cfg.port}";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
  };
}
