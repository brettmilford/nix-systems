{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.services.podman;
in
{
  options.services.podman = {
    enable = mkEnableOption "Podman" // {
      default = false;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      (if pkgs.stdenv.hostPlatform.system == "x86_64-darwin" then pkgs.unstable.podman else pkgs.podman)
      pkgs.podman-compose
      pkgs.qemu
      pkgs.xz
    ];

    homebrew = {
      enable = true;
      casks = [
        # Until: https://github.com/nixos/nixpkgs/issues/487166
        "podman-desktop"
      ];
    };

    # https://github.com/containers/podman/issues/17026
    environment.pathsToLink = [ "/share/qemu" ];

    # https://github.com/LnL7/nix-darwin/issues/432#issuecomment-1024951660
    environment.etc."containers/containers.conf".text = ''
      [engine]
      helper_binaries_dir = ["${pkgs.gvproxy}/bin"]
    '';
  };
}
