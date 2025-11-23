{
  config,
  lib,
  pkgs,
  options,
  ...
}:
{

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome = {
    enable = true;
    favoriteAppsOverride = ''
      [org.gnome.shell]
      favorite-apps=[ 'firefox.desktop', 'emacsclient.desktop', 'org.gnome.Console.desktop', 'org.gnome.Nautilus.desktop' ]
    '';
  };

  environment.systemPackages = with pkgs.gnomeExtensions; [
    appindicator
  ];

  services.udev.packages = with pkgs; [ gnome-settings-daemon ];

  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/gnome/mutter" = {
          experimental-features = [
            "scale-monitor-framebuffer" # Enables fractional scaling (125% 150% 175%)
            "variable-refresh-rate" # Enables Variable Refresh Rate (VRR) on compatible displays
            "xwayland-native-scaling" # Scales Xwayland applications to look crisp on HiDPI screens
          ];
        };
        "org/gnome/shell" = {
          enabled-extensions = [
            pkgs.gnomeExtensions.appindicator.extensionUuid
          ];
        };
      };
    }
  ];
}
