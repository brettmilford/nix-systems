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
    ./opnsense.nix
    ../common.nix
    ../cloud.nix
    ../desktop.nix
    ../zerotierone.nix
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
  environment.systemPackages = with pkgs; [
    iw
  ];

  services.deployments = {
    homeAssistant.enable = true;
    unifi.enable = true;
  };
}
