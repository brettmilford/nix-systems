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
    ../virt.nix
    ../../../deployments/unifi
    ../../../deployments/home-assistant
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
  services.logind = {
    powerKey = "poweroff";
    #powerKeyLongPress = "ignore";
    #rebootKey = "reboot";
    suspendKey = "poweroff";
    #hibernateKey = "hibernate";
    #lidSwitch = "suspend";
    #lidSwitchExternalPower = "suspend";
    #lidSwitchDocked = "ignore";
  };

  services.xserver.desktopManager.gnome = {
    enable = true;
    extraGSettingsOverrides = ''
      [org.gnome.settings-daemon.plugins.power]
      power-button-action='poweroff'
      sleep-inactive-ac-type='nothing'
      sleep-inactive-battery-type='nothing'

      [org.gnome.desktop.session]
      idle-delay=uint32 0
    '';
  };

  #programs.dconf.profiles = {
  #  user.databases = [{
  #    settings = {
  #      "org/gnome/settings-daemon/plugins/power" = {
  #        power-button-action = "poweroff";
  #        sleep-inactive-ac-type = "nothing";
  #        sleep-inactive-battery-type = "nothing";
  #      };
  #      "org/gnome/desktop/session" = {
  #        idle-delay = lib.hm.gvariant.mkUint32 0;
  #      };
  #    };
  #  }];
  #};

  environment.systemPackages = with pkgs; [
    iw
    pciutils
    usbutils
    dig
  ];

  # common reverse proxy
  security.acme = {
    acceptTerms = true;
    defaults.email = "admin+acme@example.org";
  };
  networking.firewall.allowedTCPPorts = [80 443];

  services.openssh.settings = {
    X11Forwarding = true;
  };
}
