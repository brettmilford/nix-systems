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
  services.unpoller = {
    influxdb.disable = true;
    unifi.defaults.verify_ssl = false;
    unifi.defaults.user = "unifipoller";
    unifi.defaults.pass = cfg.secretPaths.unifipoller_pass;
  };
}
