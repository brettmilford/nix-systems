{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.monitoring-hass;
in
{
  options.services.monitoring-hass = {
    enable = lib.mkEnableOption "Home Assistant monitoring (MQTT exporter + Prometheus endpoint)";

    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "Address to listen on";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 9641;
      description = "Port to listen on";
    };

    mqttServer = lib.mkOption {
      type = lib.types.attrs;
      default = {
        address = "127.0.0.1";
        port = 1883;
      };
      description = "MQTT server configuration";
    };
  };

  config = lib.mkIf cfg.enable {
    services.prometheus.exporters.mqtt = {
      enable = true;
      inherit (cfg) listenAddress port;
      mqttAddress = cfg.mqttServer.address;
      mqttPort = cfg.mqttServer.port;
      #mqttTopic = "zigbee2mqtt/+";
      zigbee2MqttAvailability = true;
      openFirewall = true;
    };
  };
}
