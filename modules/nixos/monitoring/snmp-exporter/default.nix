{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.monitoring;
in
{
  services.prometheus.exporters.snmp = {
      configurationPath = ./snmp.yml;
      environmentFile = cfg.secretPaths.snmp_env;
  };

  services.prometheus.scrapeConfigs = [
        {
          job_name = "snmp-exporter";
          static_configs = [{
            targets = [
              "opnsense"
            ];
          }];
          metrics_path = "/snmp";
          params = {
            module = ["opnsense"];
            auth = ["public_v3"];
          };
          relabel_configs = [
            {
              source_labels = ["__address__"];
              target_label = "__param_target";
            }
            {
              source_labels = ["__param_target"];
              target_label = "instance";
            }
            {
              target_label = "__address__";
              replacement = "localhost:9116";
            }
          ];
        }
  ];
}
