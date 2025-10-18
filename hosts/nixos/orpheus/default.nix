{
  config,
  lib,
  pkgs,
  modulesPath,
  options,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../common.nix
    ../cloud.nix
    ../desktop.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "orpheus";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  services.rsnapshotBackup.enable = true;
}
