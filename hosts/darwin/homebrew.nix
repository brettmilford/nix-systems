
{
  # NOTE: `brew` cli linked at this path
  environment.systemPath = [
    "/opt/homebrew/bin"
  ];

  homebrew = {
    enable = true;

    casks = [
      "rectangle"
      "notion"
      "firefox"
      "google-chrome"
      "nextcloud"
      "xquartz"
      "zerotier-one"
      "font-iosevka"
      "font-iosevka-aile"
      "proton-pass"
      "protonvpn"
    ];
  };
}
