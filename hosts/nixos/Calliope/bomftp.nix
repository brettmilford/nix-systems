{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    lftp
  ];

  systemd.services.bomftp = {
    description = "Daily BOM download";
    wantedBy = ["timers.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "-${pkgs.lftp}/bin/lftp -e 'lcd /srv/data/nextcloud/data/brett/files/bomdata; mget anon/gen/fwo/IDQ6590*.hcs' -u anonymous, ftp.bom.gov.au";
      ExecStartPost = "/run/current-system/sw/bin/nextcloud-occ files:scan --all";
    };
  };

  systemd.timers.bomftpTimer = {
    description = "Run BOM download Timer";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "*-*-* 7:00:00";
      Persistent = true;
      Unit = "bomftp.service";
    };
  };
}
