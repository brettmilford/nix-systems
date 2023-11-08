{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      (modulesPath + "/profiles/headless.nix")
      ../common.nix
      ../cloud.nix
      ./postgresql.nix
      ./nextcloud.nix
    ];

  networking.hostName = "Calliope";
  networking.hostId = "25f4937c";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  boot.kernelParams = [ "net.ifnames=0" ];
  boot.zfs.devNodes = "/dev";
  boot.zfs.extraPools = [ "dpool" ];

  fileSystems."/srv" = {
    device = "dpool";
    fsType = "zfs";
  };

  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

}
