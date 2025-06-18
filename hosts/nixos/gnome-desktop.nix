{
  config,
  lib,
  pkgs,
  options,
  ...
}: {

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome = {
    enable = true;
    favoriteAppsOverride = ''
      [org.gnome.shell]
      favorite-apps=[ 'firefox.desktop', 'org.gnome.Console.desktop', 'org.gnome.Nautilus.desktop' ]
    '';
  };

  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-photos
      gnome-tour
      snapshot
      gedit # text editor
      cheese # webcam tool
      gnome-music
      gnome-terminal
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
    ])
    ++ (with pkgs.gnome; [
    ]);

  environment.systemPackages = with pkgs; [
    firefox
    bitwarden
    gnomeExtensions.appindicator
  ];

  services.udev.packages = with pkgs; [gnome-settings-daemon];

}
