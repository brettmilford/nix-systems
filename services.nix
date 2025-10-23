let
  domain = "cirriform.au";
in
{
  inherit domain;
  rsyslog = {
    hosts = [ "eurydice" "terpsichore" ];
  };

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

  immich = {
    hosts = [ "calliope" ];
    fqdn = "immich.${domain}";
    config = {
      dataPath = "/immich";
    };
  };

  paperless = {
    hosts = [ "calliope" ];
    fqdn = "paperless.${domain}";
    config = {
      dataPath = "/paperless";
    };
  };

  monitoring = {
    hosts = [ "eurydice" ];
    fqdn = "metrics.${domain}";
  };

  hass = {
    hosts = [ "eurydice" ];
    fqdn = "hass.${domain}";
  };

  nixBuildMachines = [ "calliope" "orpheus" "terpsichore" ];

  backup = {
    config = {
      repos = {
        # Eurydice's data backed up to terpsichore and calliope
        eurydice = {
          targets = [ "terpsichore" "calliope" ];
        };

        calliope = {
          targets = [ "terpsichore" ];
        };

        # Terpsichore's data backed up to calliope
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
