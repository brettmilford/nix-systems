{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.unifi-controller;
  stackName = "unifi";
  compose = pkgs.writeTextDir "compose.yaml" "${builtins.readFile ./compose.yaml}";
  init-mongo = pkgs.writeTextDir "init-mongo.sh" "${builtins.readFile ./init-mongo.sh}";
  composeStack = pkgs.symlinkJoin {
    name = "unifi-compose";
    paths = [
      compose
      init-mongo
    ];
  };
in
{
  config = lib.mkIf (cfg.enable && cfg.docker-compose) {
    services.gateway.enable = true;

    environment.systemPackages = with pkgs; [
      podman
    ];

    networking.firewall.allowedTCPPorts = [ 8080 ];
    networking.firewall.allowedUDPPorts = [
      3478
      10001
    ];

    systemd.services."podman-${stackName}" = {
      description = "Podman Compose Stack Service for ${stackName}";
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      serviceConfig = {
        Environment = "PATH=${pkgs.podman}/bin:${pkgs.podman-compose}/bin:/run/wrappers/bin:/usr/bin:/bin";
        Type = "simple";
        ExecStart = "${pkgs.podman-compose}/bin/podman-compose --env-file ${cfg.secretPaths.dockerEnv} -f ${composeStack}/compose.yaml up";
        ExecStop = "${pkgs.podman-compose}/bin/podman-compose -f ${composeStack}/compose.yaml down";
        Restart = "always";
        User = "root";
        WorkingDirectory = "${composeStack}";
      };

      wantedBy = [ "multi-user.target" ];
    };

    services.nginx.virtualHosts."${cfg.fqdn}" = {
      addSSL = true;
      locations."/" = {
        proxyPass = "https://127.0.0.1:8443/";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header        Host $host;
          proxy_set_header        X-Real-IP $remote_addr;
          proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header        X-Forwarded-Proto $scheme;
          proxy_set_header        X-Forwarded-Host $host;
          proxy_set_header        X-Forwarded-Server $host;
        '';
      };
    };
  };
}
