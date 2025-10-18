{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./rsnapshot.nix
    ./build-vm.nix
  ];

  system.stateVersion = "25.05";
  nix = {
    extraOptions = ''
      extra-platforms = aarch64-linux x86_64-linux
      experimental-features = nix-command flakes
    '';
    settings.auto-optimise-store = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
  programs.git = {
    enable = true;
    config = {
      safe.directory = [
        "/etc/nixos"
        "/etc/nixos/config"
        "/etc/nixos/deployments"
      ];
    };
  };
}
