{ config, lib, pkgs, ... }:
let
  cfg = config.services.monitoring;
in
{
    # Alertmanager configuration
    services.prometheus.alertmanager = {
      port = cfg.ports.alertmanager;
      listenAddress = "localhost";
      webExternalUrl = "http://${cfg.fqdn}/alertmanager/";
      extraFlags = [ "--web.route-prefix=/" ];

      # TODO: verify
      configuration = {
        global = {
          smtp_smarthost = "localhost:587";
          smtp_from = "alerts@${cfg.fqdn}";
        };

        route = {
          group_by = [ "alertname" ];
          group_wait = "10s";
          group_interval = "10s";
          repeat_interval = "1h";
          receiver = "web.hook";
        };

        receivers = [
          {
            name = "web.hook";
            # Add your notification methods here
            # webhook_configs = [{
            #   url = "http://example.com/webhook";
            # }];
            # email_configs = [{
            #   to = "admin@example.com";
            #   subject = "Alert: {{ .GroupLabels.alertname }}";
            #   body = "{{ range .Alerts }}{{ .Annotations.description }}{{ end }}";
            # }];
          }
        ];
      };
    };

  services.nginx.virtualHosts."${cfg.fqdn}".locations."/alertmanager/" = lib.mkIf cfg.enableAlertManager {
    proxyPass = "http://localhost:${toString cfg.ports.alertmanager}/";
  };
}
