{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.unifi-controller;
in
{
  imports = [
    ../gateway.nix
    ./docker.nix
  ];

  options.services.unifi-controller = {
    enable = lib.mkEnableOption "UniFi Network Controller with nginx reverse proxy";

    fqdn = lib.mkOption {
      type = lib.types.str;
      description = "FQDN for UniFi Network Application";
    };

    docker-compose = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Use Docker Compose instead of native NixOS service";
    };
    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Secret file paths";
    };
  };

  config = lib.mkIf (cfg.enable && !cfg.docker-compose) {
    services.gateway.enable = true;

    nixpkgs.config.allowUnfreePredicate =
      pkg:
      builtins.elem (lib.getName pkg) [
        "unifi-controller"
        "mongodb"
        "mongodb-ce"
      ];

    # Configure upstream UniFi service
    services.unifi = {
      enable = true;
      openFirewall = true;
      unifiPackage = pkgs.unifi8;
      mongodbPackage = pkgs.mongodb;
      initialJavaHeapSize = 1024;
      maximumJavaHeapSize = 1024;
    };

    # Override systemd service type for unifi8
    systemd.services.unifi = {
      serviceConfig = {
        Type = lib.mkForce "simple";
        TimeoutSec = "5min";
      };
    };

    # Nginx virtual host with SSL
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
