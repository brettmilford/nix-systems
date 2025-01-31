
{
  # NOTE: `brew` cli linked at this path
  environment.systemPath = [
    "/opt/homebrew/bin"
  ];

  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";

    taps = [
      "railwaycat/emacsmacport"
    ];

    brews = [
      {
        name = "emacs-mac";
        args = ["with-native-comp"];
      }
    ];

    casks = [
      "rectangle"
      "notion"
      "firefox"
      "google-chrome"
      "nextcloud"
      "anki"
      "xquartz"
      "gimp"
      "krita"
      "inkscape"
      "obsidian"
      "iTerm2"
      "font-iosevka"
      "font-iosevka-aile"
      "proton-pass"
      "protonvpn"
      "proton-mail"
      "proton-drive"
    ];
  };
}
