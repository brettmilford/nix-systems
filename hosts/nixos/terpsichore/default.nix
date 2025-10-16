{
  self,
  config,
  lib,
  pkgs,
  modulesPath,
  options,
  lanzaboote,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ../common.nix
    ../cloud.nix
  ];

  networking.hostName = "terpsichore";
  networking.hostId = "37393231";

  # Secure boot support
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl";
  };

  boot.loader.efi.canTouchEfiVariables = true;

  # ZFS Support
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.forceImportRoot = false;
  boot.zfs.extraPools = [ "dpool" ];

  # LUKS
  boot.initrd.luks.devices."cryptroot" = {
    device = "/dev/disk/by-uuid/07ef0ed3-2eb0-4ece-b3d2-896150fb1e85";
  };

  # TPM2 support
  boot.initrd.systemd.enable = true;
  boot.initrd.systemd.tpm2.enable = true;
  security.tpm2 = {
    enable = true;
    pkcs11.enable = true;
    tctiEnvironment.enable = true;
  };

  swapDevices = [
    {
      device = "/dev/disk/by-partuuid/2c708d5b-b160-4496-92d7-2c8130600f0e";
      randomEncryption.enable = true;
    }
  ];

  services.zfs.autoScrub.enable = true;

  # ZFS encryption support
  systemd.services.zfs-load-key = {
    description = "Load ZFS encryption keys";
    wantedBy = [ "zfs-mount.service" ];
    before = [ "zfs-mount.service" ];
    after = [ "systemd-modules-load.service" ];
    unitConfig = {
      DefaultDependencies = "no";
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.zfs}/bin/zfs load-key -a";
    };
  };

  networking.useDHCP = lib.mkDefault true;

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
}
