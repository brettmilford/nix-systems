{ config, lib, pkgs, nixpkgs, ... }:

{
  imports = [
    (nixpkgs + "/nixos/modules/profiles/qemu-guest.nix")
    (nixpkgs + "/nixos/modules/profiles/headless.nix")
    ../common.nix
    ../cloud.nix
  ];

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

  networking.hostName = "dev";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

}
