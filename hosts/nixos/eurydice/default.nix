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
    ../virt.nix
    ../../../deployments
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
  environment.systemPackages = with pkgs; [
    iw
  ];

  # common reverse proxy
  security.acme = {
	  acceptTerms = true;
	  defaults.email = "admin+acme@example.org";
  };
  networking.firewall.allowedTCPPorts = [80 443];

  # Enable container-based service deployments
  services.deployments = {
    enable = true;
    
    # Enable specific services
    elasticsearch.enable = true;
    homeAssistant.enable = true;
    unifi.enable = true;
  };


}
