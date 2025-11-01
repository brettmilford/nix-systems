{
  self,
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
    ./backup.nix
    ../common.nix
    ../cloud.nix
  ];

  system.stateVersion = "24.05";

  networking.hostName = "eurydice";
  networking.hostId = "04ca88ad";
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.forceImportRoot = false;

  # TPM2 support
  boot.initrd.systemd.enable = true;
  boot.initrd.systemd.tpm2.enable = true;
  security.tpm2 = {
    enable = true;
    pkcs11.enable = true;
    tctiEnvironment.enable = true;
  };

  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  environment.systemPackages = with pkgs; [
    wpa_supplicant
    iw
    pciutils
    usbutils
    tpm2-tools
    sbctl
    smartmontools
    nvme-cli
  ];

  services.openssh.settings = {
    X11Forwarding = true;
  };

}
