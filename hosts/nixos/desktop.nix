{ config, lib, pkgs, options, ... }:

{
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

  console.packages = options.console.packages.default ++ [ pkgs.terminus_font ];

  services.xserver = {
    enable = true;
    layout = "au";
  };
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome = {
    enable = true;
    favoriteAppsOverride = ''
      [org.gnome.shell]
      favorite-apps=[ 'firefox.desktop', 'org.gnome.Console.desktop', 'org.gnome.Nautilus.desktop' ]
    '';
  };

  environment.gnome.excludePackages = (with pkgs; [
    gnome-photos
    gnome-tour
    snapshot
  ]) ++ (with pkgs.gnome; [
    cheese # webcam tool
    gnome-music
    gnome-terminal
    gedit # text editor
    epiphany # web browser
    geary # email reader
    #evince # document viewer
    gnome-characters
    totem # video player
    tali # poker game
    iagno # go game
    hitori # sudoku game
    atomix # puzzle game
    yelp # Help view
    gnome-contacts
    gnome-initial-setup
    gnome-maps
  ]);

  environment.systemPackages = with pkgs; [
    firefox
    bitwarden
    gnomeExtensions.appindicator
  ];

  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];

  sound.enable = true;
  hardware.pulseaudio.enable = false;
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
    nssmdns = true;
    openFirewall = true;
  };
}
