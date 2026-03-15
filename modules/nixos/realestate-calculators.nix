{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.realestate-calculators;
  flake = builtins.getFlake "git+ssh://git@calliope/~/recalc.git?rev=${cfg.rev}";
  pkg = flake.packages.${pkgs.system}.default;
in
{
  imports = [
    ./gateway.nix
  ];

  options.services.realestate-calculators = {
    enable = mkEnableOption "realestate-calculators";
    fqdn = mkOption {
      type = types.str;
      description = "FQDN for the nginx virtualHost";
    };
    rev = mkOption {
      type = types.str;
      description = "Pinned git revision of realestate-calculators flake";
    };
  };

  config = mkIf cfg.enable {
    services.gateway.enable = true;
    services.nginx.virtualHosts.${cfg.fqdn} = {
      locations."/" = {
        root = "${pkg}";
        tryFiles = "$uri $uri/ /index.html";
        extraConfig = ''
          expires 1h;
          add_header Cache-Control "public, no-transform";
        '';
      };
    };
  };
}
