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
    hosts = [ "terpsichore" ];
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
    hosts = [ "terpsichore" ];
    fqdn = "paperless.${domain}";
    config = {
      dataPath = "/paperless";
    };
  };

  monitoring = {
    hosts = [
      "terpsichore"
    ];
    fqdn = "dash.${domain}";
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
        {
          # opnsense
          publicKey = "m+e6WbpDrsbwUPc88Q3ZESt+qfawUxkAzC/02jeV0G8=";
          allowedIPs = [
            "172.16.1.1/32"
            "192.168.0.0/16"
          ];
        }
      ];
    };
  };

  garmin-collect = {
    hosts = [ "terpsichore" ];
    fqdn = "measure.${domain}";
    config = {
      rev = "40957d00c426d3a05d7d411858f1fe3663f30cff";
      measurePort = 8642;
    };
  };

  account-service = {
    hosts = [ "terpsichore" ];
    fqdn = "account.${domain}";
    config.rev = "a8a85b0743969dea9080e4bb075d33ed3365902f";
  };

  realestate-calculators = {
    hosts = [ "terpsichore" ];
    fqdn = "recalc.${domain}";
    config.rev = "cf2a5a03c01b3932e99075cbeb429a792bd7faf6";
  };

  opencode-web = {
    hosts = [ "terpsichore" ];
    fqdn = "code.${domain}";
    config = {
      dataPath = "opencode-web";
    };
  };

  srht = {
    hosts = [ "terpsichore" ];
    config = {
      dataPath = "sourcehut";
    };
  };

  ftp = {
    hosts = [ "terpsichore" ];
    fqdn = "ftp.${domain}";
    config = {
      dataPath = "ftp";
    };
  };

  nfs-server = {
    hosts = [ "terpsichore" ];
  };

  git-server = {
    hosts = [ "calliope" ];
    config = {
      dataPath = "/git";
      extraAuthorizedKeys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINdBecv6oeIpHd2kYJw0qVDKbAC1ydBybnVO3VJgbu7M git@opnsense"
      ];
    };
  };

  mail-relay = {
    hosts = [
      "calliope"
      "terpsichore"
    ];
  };
}
