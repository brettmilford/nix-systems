let
  domain = "cirriform.au";
in
{
  inherit domain;
  auth = {
    hosts = [ "calliope" ];
    fqdn = "auth.${domain}";
  };

  cloud = {
    hosts = [ "calliope" ];
    fqdn = "cloud.${domain}";
    config = {
      dataPath = "/nextcloud";
    };
  };

  photos = {
    hosts = [ "calliope" ];
    fqdn = "immich.${domain}";
    config = {
      dataPath = "/immich";
    };
  };

  paperless-ngx = {
    hosts = [ "calliope" ];
    fqdn = "paperless.${domain}";
    config = {
      dataPath = "/paperless";
    };
  };

  monitoring = {
    hosts = [
      "terpsichore"
    ];
    fqdn = "metrics.${domain}";
  };

  hass = {
    hosts = [ "eurydice" ];
    fqdn = "hass.${domain}";
  };

  unifi-controller = {
    hosts = [ "eurydice" ];
    fqdn = "unifi.${domain}";
  };

  nixBuildMachines = [
    "calliope"
    "terpsichore"
    "orpheus"
  ];

  backup = {
    # Repo servers
    hosts = [
      "terpsichore"
      "calliope"
    ];
    config = {
      repos = {
        # Eurydice's data is backed up to terpsichore and calliope
        eurydice = {
          targets = [
            "terpsichore"
            "calliope"
          ];
        };
        calliope = {
          targets = [ "terpsichore" ];
        };
        terpsichore = {
          targets = [ "calliope" ];
        };
        orpheus = {
          targets = [ "terpsichore" ];
        };
        thamrys = {
          targets = [ "terpsichore" ];
        };
      };
    };
  };
}
