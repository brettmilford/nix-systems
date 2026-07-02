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
      packages = [ pkgs.nodejs ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAlB/hd55JJCoIb8EDBvvwfrdGtTOli5H+d+3o0wqxYR brett@thamrys"
      ];
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
          "OPENCODE_ENABLE_EXA=1"
        ];
        ExecStart = "/run/current-system/sw/bin/bash -lc '${pkgs.opencode}/bin/opencode web --port ${toString cfg.port} --hostname 127.0.0.1 --cors https://${cfg.fqdn}'";
        Restart = "on-failure";
        RestartSec = "5s";
      };
    };

    security.sudo.extraRules = [
      {
        users = [ "opencode" ];
        commands = [
          {
            command = "/run/current-system/sw/bin/systemctl restart opencode-web.service";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];

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

          proxy_buffering off;
          proxy_cache off;
          proxy_set_header X-Accel-Buffering no;
          chunked_transfer_encoding off;

          proxy_hide_header Content-Security-Policy;
          add_header Content-Security-Policy "default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval' 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data: https:; font-src 'self' data:; media-src 'self' data:; connect-src 'self' ws: wss: data: blob:; worker-src 'self' blob:;";
        '';
      };
    };
  };
}
