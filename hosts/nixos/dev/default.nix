{
  config,
  lib,
  pkgs,
  modulesPath,
  hostname,
  ...
}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    (modulesPath + "/profiles/headless.nix")
    ../common.nix
    ../cloud.nix
  ];

 system.stateVersion = "23.05";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  networking.hostName = hostname;
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
}
