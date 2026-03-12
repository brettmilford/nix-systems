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
    hosts = [ "terpsichore" ];
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
    # "orpheus" # 2026-02-20: Offline
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
        # 2026-02-20: Offline
        # orpheus = {
        #   targets = [ "terpsichore" ];
        # };
        thamrys = {
          targets = [ "terpsichore" ];
        };
      };
    };
  };

  wg-gateway = {
    hosts = [ "calliope" ];
    config = {
      externalIP = "10.0.0.33";
      peers = [
        {
          publicKey = "EDy72uHnnxYQhiiwjSo2ZQa05QsUHcbGWc2IuXo0Qmg=";
          allowedIPs = [ "172.16.0.2/32" ];
        }
        {
          publicKey = "vYRA5xUEocCBlB2LlUP5EHRzriUdCzwWsU3idpXZuRM=";
          allowedIPs = [ "172.16.0.3/32" ];
        }
      ];
    };
  };

  git-server = {
    hosts = [ "calliope" ];
    config = {
      dataPath = "/git";
    };
  };
}
