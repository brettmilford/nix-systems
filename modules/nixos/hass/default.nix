{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.hass;
in
{
  imports = [
    ../gateway.nix
  ];

  options.services.hass = {
    enable = lib.mkEnableOption "Home Assistant";

    fqdn = lib.mkOption {
      type = lib.types.str;
      description = "FQDN for Home Assistant";
    };
  };

  config = lib.mkIf cfg.enable {
    # Stub implementation - to be filled in later
    services.gateway.enable = true;
  };
}
