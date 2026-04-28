{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.hass;
in
{
  imports = [
    ../gateway.nix
  ];

  options.services.hass = {
    enable = lib.mkEnableOption "Home Assistant with MQTT and Zigbee2MQTT";

    fqdn = lib.mkOption {
      type = lib.types.str;
      description = "FQDN for Home Assistant";
    };

    nfsMounts = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              description = "Mount name (used as media subdirectory)";
            };
            server = lib.mkOption {
              type = lib.types.str;
              description = "NFS server IP address";
            };
            remotePath = lib.mkOption {
              type = lib.types.str;
              description = "Remote NFS export path";
            };
          };
        }
      );
      default = [ ];
      description = "NFS shares to mount as media directories";
    };

    secretPaths = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Paths to secret files";
    };
  };

  config = lib.mkIf cfg.enable {
    services.gateway.enable = true;

    # Firewall configuration
    networking.firewall = {
      allowedTCPPorts = [
        8123 # Home Assistant
        1883 # MQTT
        9001 # MQTT websockets
        8081 # Zigbee2MQTT
        # Sonos discovery ports
        1400
        1443
        21063
        21064
        21065
      ];
      allowedUDPPorts = [
        5353 # mDNS
        1900 # UPnP
      ];
    };

    # Home Assistant configuration
    services.home-assistant = {
      enable = true;
      extraComponents = [
        "default_config"
        "isal"
        "frontend"
        "homekit"
        "mqtt"
        "sonos"
        "hue"
        "met" # Weather
        "unifiprotect"
        "zha"
        "homekit_controller"
        "enphase_envoy"
        "ecovacs"
        "radio_browser"
        "unifi"
        "forecast_solar"
        "reolink"
        "anthropic"
        "google_translate"
        "local_todo"
        "prometheus"
      ];

      config = {
        # Core configuration
        default_config = { };

        frontend = {
          themes = "!include_dir_merge_named themes";
        };

        # HTTP configuration for reverse proxy
        http = {
          use_x_forwarded_for = true;
          trusted_proxies = [
            "127.0.0.1"
            "::1"
          ];
        };

        # UI-generated configuration includes
        automation = "!include automations.yaml";
        script = "!include scripts.yaml";
        scene = "!include scenes.yaml";
        input_boolean = "!include input_boolean.yaml";

        # HomeKit configuration
        homekit = [
          {
            name = "HASS Bridge";
            advertise_ip = "192.168.10.64";
          }
        ];

      };

      configDir = "/var/lib/hass";

      config.homeassistant.media_dirs = lib.listToAttrs (
        map (mount: {
          name = mount.name;
          value = "/var/lib/hass/media/${mount.name}";
        }) cfg.nfsMounts
      );
    };

    # NFS media mounts — mount outside /var/lib/hass so HA backups
    # don't traverse them; symlink into media dir for HA access.
    fileSystems = lib.listToAttrs (
      map (mount: {
        name = "/srv/media/${mount.name}";
        value = {
          device = "${mount.server}:${mount.remotePath}";
          fsType = "nfs";
          options = [
            "ro"
            "soft"
            "timeo=30"
            "_netdev"
          ];
        };
      }) cfg.nfsMounts
    );

    # Create UI automation files if they don't exist, and symlink
    # NFS media mounts into the hass media directory.
    systemd.tmpfiles.rules =
      [
        "f /var/lib/hass/automations.yaml 0644 hass hass - []"
        "f /var/lib/hass/scripts.yaml 0644 hass hass - []"
        "f /var/lib/hass/scenes.yaml 0644 hass hass - []"
        "f /var/lib/hass/input_boolean.yaml 0644 hass hass - {}"
        "d /var/lib/hass/themes 0755 hass hass - -"
      ]
      ++ map (mount: "L /var/lib/hass/media/${mount.name} - - - - /srv/media/${mount.name}") cfg.nfsMounts;

    # MQTT Broker (Mosquitto)
    # mosquitto_sub -h 127.0.0.1 -v -t "homeassistant/#"
    # mosquitto_pub -h 127.0.0.1 -t homeassistant/switch/1/on -m "Switch is ON"
    services.mosquitto = {
      enable = true;
      persistence = false;
      # https://github.com/NixOS/nixpkgs/issues/27130#issuecomment-986075006
      listeners = [
        {
          port = 1883;
          address = "localhost";
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
          users = { };
          acl = [
            "topic readwrite #"
            "pattern readwrite #"
          ];
        }
      ];
    };

    # Zigbee2MQTT
    services.zigbee2mqtt = {
      enable = true;
      settings = {
        homeassistant = {
          enable = true;
          discovery_topic = "homeassistant";
          status_topic = "homeassistant/status";
        };
        frontend.port = 8081;
        mqtt = {
          base_topic = "zigbee2mqtt";
          server = "mqtt://localhost";
        };
        availability.enable = true;
        serial = {
          port = "/dev/serial/by-id/usb-Itead_Sonoff_Zigbee_3.0_USB_Dongle_Plus_V2_d80fe62d653aef11a73c321455516304-if00-port0";
          baudrate = 115200;
          adapter = "ezsp";
          ezspVersion = 8;
        };
        advanced = {
          homeassistant_legacy_entity_attributes = false;
          legacy_api = false;
          legacy_availability_payload = false;
        };
        device_options.legacy = false;
        devices = {
          "0x70c59cfffee75325" = {
            friendly_name = "Dryer";
          };
          "0xd44867fffed4e6f8" = {
            friendly_name = "Front Gate Sensor";
          };
          "0xd44867fffeb3d18e" = {
            friendly_name = "Garrage Door Sensor";
          };
          "0xd44867fffed44f3f" = {
            friendly_name = "Side Gate Sensor";
          };
          "0x70c59cfffef078f3" = {
            friendly_name = "Pool Pump 2";
          };
          "0x28dba7fffe553ce3" = {
            friendly_name = "Floor lamp";
          };
          "0x54ef441000e7a303" = {
            friendly_name = "Garage relay";
          };
          "0x54ef441000e79820" = {
            friendly_name = "Gate relay";
          };
        };
      };
    };

    # Add user to required groups for USB device access
    users.users.zigbee2mqtt = {
      extraGroups = [
        "dialout"
        "tty"
        "uucp"
      ];
    };

    # Nginx virtual hosts
    services.nginx.virtualHosts."hass.internal" = {
      enableACME = false;
      addSSL = false;
      locations."/" = {
        proxyPass = "http://localhost:8123/";
        extraConfig = ''
          proxy_set_header    Upgrade     $http_upgrade;
          proxy_set_header    Connection  "upgrade";
          client_max_body_size 100M;
        '';
      };
    };

    services.nginx.virtualHosts."z2m.${cfg.fqdn}" = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:8081/";
        extraConfig = ''
          proxy_set_header    Upgrade     $http_upgrade;
          proxy_set_header    Connection  "upgrade";
        '';
      };
    };

    # Cloudflare tunnel configuration
    services.cloudflared = {
      enable = true;
      tunnels = {
        "c152d57e-441c-4cea-a193-0d493116ccac" = {
          credentialsFile = cfg.secretPaths.cfdCredentialsFile;
          default = "http_status:404";
          ingress = {
            "${cfg.fqdn}" = {
              service = "http://localhost:8123";
            };
          };
        };
      };
    };

    # Cloudflared package
    environment.systemPackages = [ pkgs.cloudflared ];

    # Home Assistant Prometheus integration
    services.home-assistant.config.prometheus = {
      namespace = "homeassistant";
    };
  };
}
