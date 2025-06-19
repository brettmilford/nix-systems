{
  config,
  lib,
  pkgs,
  options,
  ...
}: {

  imports = [
    ./gnome-desktop.nix
  ];

  networking.networkmanager.enable = true;

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_AU.UTF-8";
    LC_IDENTIFICATION = "en_AU.UTF-8";
    LC_MEASUREMENT = "en_AU.UTF-8";
    LC_MONETARY = "en_AU.UTF-8";
    LC_NAME = "en_AU.UTF-8";
    LC_PAPER = "en_AU.UTF-8";
    LC_TELEPHONE = "en_AU.UTF-8";
    LC_TIME = "en_AU.UTF-8";
  };

  console.packages = options.console.packages.default ++ [pkgs.terminus_font];

  services.xserver = {
    enable = true;
    xkb.layout = "au";
  };

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  powerManagement.powerDownCommands = ''
    echo enabled > /sys/bus/usb/devices/usb1/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb2/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb3/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb4/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb5/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb6/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb7/power/wakeup
    echo enabled > /sys/bus/usb/devices/usb8/power/wakeup
  '';

  services.printing.enable = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Experimental = true;
      };
    };
  };

  services.blueman.enable = true;

  services.dbus.enable = true;

  environment.systemPackages = with pkgs; [
    bluez
    bluez-tools
    iw
    pciutils
    usbutils
    dig
  ];
}
