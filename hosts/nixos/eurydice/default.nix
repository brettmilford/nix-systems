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
    ../zerotierone.nix
  ];

  boot.loader.efi.canTouchEfiVariables = false;

  boot.loader.grub = {
	enable = true;
	zfsSupport = true;
	efiSupport = true;
	efiInstallAsRemovable = true;
	mirroredBoots = [
		{ devices = ["nodev"]; path = "/boot";}
	];
  };

  networking.hostName = "eurydice";
  networking.hostId = "04ca88ad";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  services.xserver.displayManager.gdm.autoSuspend = false;
}
