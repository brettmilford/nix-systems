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
    ../../../deployments
    "${self}/modules/monitoring"
    "${self}/modules/backup.nix"
  ];

  networking.hostName = "eurydice";
  networking.hostId = "04ca88ad";

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

  #services.xserver.displayManager.gdm.autoSuspend = false;
  #services.logind = {
  #  powerKey = "poweroff";
  #  #powerKeyLongPress = "ignore";
  #  #rebootKey = "reboot";
  #  suspendKey = "poweroff";
  #  #hibernateKey = "hibernate";
  #  #lidSwitch = "suspend";
  #  #lidSwitchExternalPower = "suspend";
  #  #lidSwitchDocked = "ignore";
  #};

  #services.xserver.desktopManager.gnome = {
  #  enable = true;
  #  extraGSettingsOverrides = ''
  #    [org.gnome.settings-daemon.plugins.power]
  #    power-button-action='poweroff'
  #    sleep-inactive-ac-type='nothing'
  #    sleep-inactive-battery-type='nothing'

  #    [org.gnome.desktop.session]
  #    idle-delay=uint32 0
  #  '';
  #};

  services.openssh.settings = {
    X11Forwarding = true;
  };

  services.deployments = {
    homeAssistant.enable = true;
    unifi.enable = true;
  };

  services.monitoring = {
    enable = true;
    domain = "monit.internal";
    enableUnpoller = true;
  };

  services.backup.enable = true;
}
