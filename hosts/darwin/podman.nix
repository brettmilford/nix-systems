{ config, lib, pkgs, ... }:
let
  cfg = config.services.podman;

  inherit (lib)
    mkEnableOption
    mkIf
  ;
in
{
  options.services.podman = {
    enable = mkEnableOption "Podman" // {
      default = false;
    };
  };

  config = mkIf cfg.enable {
     environment.systemPackages =
      [
        pkgs.podman
        pkgs.podman-compose
        pkgs.qemu
        pkgs.xz
      ];

    # https://github.com/containers/podman/issues/17026
    environment.pathsToLink = [ "/share/qemu" ];

    # https://github.com/LnL7/nix-darwin/issues/432#issuecomment-1024951660
    environment.etc."containers/containers.conf".text = ''
            [engine]
            helper_binaries_dir = ["${pkgs.gvproxy}/bin"]
          '';
    };
}
