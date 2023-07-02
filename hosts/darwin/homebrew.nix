{
  # NOTE: `brew` cli linked at this path
  #environment.systemPath = [
  #  "/opt/homebrew/bin"
  #];

   homebrew = {
    enable = true;
    onActivation.cleanup = "zap";

    taps = [
      "homebrew/cask"
    ];

    casks = [
      "firefox"
      "google-chrome"
      "nextcloud"
      "bitwarden"
      "anki"
      "xquartz"
      "gimp"
      "krita"
      "inkscape"
      "obsidian"
      "iTerm2"
    ];
  };
}
