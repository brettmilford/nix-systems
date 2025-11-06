
{
  # NOTE: `brew` cli linked at this path
  environment.systemPath = [
    "/opt/homebrew/bin"
  ];

  homebrew = {
    enable = true;

    casks = [
      "rectangle"
      "proton-pass"
      "zerotier-one"
      "nextcloud"
      "firefox"
      "google-chrome"
      "xquartz"
      "notion"
    ];
  };
}
