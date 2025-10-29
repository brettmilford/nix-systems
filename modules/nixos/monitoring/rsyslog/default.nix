{
  config,
  lib,
  pkgs,
  nodes,
  ...
}:

let
  cfg = config.services.monitoring-rsyslog;
in
{
  options.services.monitoring-rsyslog = {
    enable = lib.mkEnableOption "rsyslog client for monitoring";

    servers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "List of monitoring servers to send logs to";
    };
  };

  config = lib.mkIf cfg.enable {
    services.rsyslogd = {
      enable = true;
      # nix eval --json '.#nixosConfigurations.calliope.config.services.rsyslogd.defaultConfig' | jq -r
      defaultConfig = ''
        # Rate limit syslog source
        $SystemLogRateLimitInterval 5
        $SystemLogRateLimitBurst 50000
        $SystemLogRateLimitSeverity 5

        # Has default rate limiting
        # https://www.rsyslog.com/doc/configuration/modules/imjournal.html
        $ModLoad imjournal

        $ModLoad imfile
        # Nginx access log
        $InputFileName /var/log/nginx/*access.log
        $InputFileTag nginx-access:
        $InputFileStateFile nginx-access-state
        $InputFileSeverity info
        $InputFileFacility local1
        $InputRunFileMonitor

        # Nginx error log
        $InputFileName /var/log/nginx/*error.log
        $InputFileTag nginx-error:
        $InputFileStateFile nginx-error-state
        $InputFileSeverity error
        $InputFileFacility local1
        $InputRunFileMonitor

        ${lib.concatMapStringsSep "\n" (
          server:
          let
            serverIP = nodes.${server}.ip or server;
          in
          "# Forward all logs to remote rsyslog server ${server}\n*.* @@${serverIP}:1514"
        ) cfg.servers}
      '';
    };
  };
}
