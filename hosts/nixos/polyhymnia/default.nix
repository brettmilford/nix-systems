{
  config,
  lib,
  pkgs,
  self,
  modulesPath,
  hostname,
  nodes,
  services,
  users,
  ...
}:
let
  thisNode = nodes.${hostname};
in
{
  imports = [
    ./hardware-configuration.nix
    ../common.nix
    ../cloud.nix
    ../desktop.nix
  ];

  system.stateVersion = "25.05";

  networking.hostName = hostname;
  networking.domain = thisNode.domain;
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  networking.hostId = "36653234";
  boot.initrd.luks.devices."cryptroot" = {
    device = "/dev/disk/by-uuid/48c3257e-825f-4c7b-a108-f9eac7270c32";
  };

  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoScrub.enable = true;
  boot.zfs.forceImportRoot = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  environment.systemPackages = with pkgs; [
    git
    smartmontools
    nvme-cli
    vim
    curl
    powertop
  ];
}
