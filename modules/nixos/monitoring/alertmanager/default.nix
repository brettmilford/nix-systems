{ config, lib, pkgs, ... }:
let
  cfg = config.services.monitoring;
in
{
  services.prometheus.alertmanager = {
    port = cfg.ports.alertmanager;
    listenAddress = "localhost";
    webExternalUrl = "http://${cfg.fqdn}/alertmanager/";
    extraFlags = [ "--web.route-prefix=/" ];

    configuration = {
      global = {
        smtp_smarthost = "localhost:25";
        smtp_from = "admin@cirriform.au";
        smtp_require_tls = false;
      };

      route = {
        group_by = [ "alertname" ];
        group_wait = "30s";
        group_interval = "5m";
        repeat_interval = "4h";
        receiver = "email";
      };

      receivers = [
        {
          name = "email";
          email_configs = [
            {
              to = "brettmilford@gmail.com";
              send_resolved = true;
            }
          ];
        }
      ];
    };
  };

  services.nginx.virtualHosts."${cfg.fqdn}".locations."/alertmanager/" = lib.mkIf cfg.enableAlertManager {
    proxyPass = "http://localhost:${toString cfg.ports.alertmanager}/";
  };
}
