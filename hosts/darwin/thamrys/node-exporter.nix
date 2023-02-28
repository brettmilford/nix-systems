{ pkgs, ... }:
{
  age.secrets."node_exporter_config" = {
    file = ../../../secrets/node_exporter_config.age;
    path = "/etc/node-exporter/config.yaml";
  };

  age.secrets."node_exporter.crt" = {
    file = ../../../secrets/node_exporter.crt.age;
    path = "/etc/node-exporter/node_exporter.crt";
  };

  age.secrets."node_exporter.key" = {
    file = ../../../secrets/node_exporter.key.age;
    path = "/etc/node-exporter/node_exporter.key";
  };

  environment.systemPackages = [ pkgs.prometheus-node-exporter ];

  launchd.agents.node-exporter = {
    command =  "${pkgs.prometheus-node-exporter}/bin/node_exporter --web.config.file=/etc/node-exporter/config.yaml";
    serviceConfig.KeepAlive = true;
    serviceConfig.RunAtLoad = true;
  };
}
