{
  config,
  lib,
  pkgs,
  ...
}: {
  fileSystems."/srv/data/lacie" = {
    device = "/dev/disk/by-uuid/ed490ab1-c523-4b18-854d-f016eab2b950";
    fsType = "btrfs";
    options = [
      "nosuid"
      "nodev"
      "nofail"
      "noauto"
      "x-gvfs-show"
      "compress"
      "x-systemd.automount"
      "x-systemd.idle-timeout=10m"
    ];
  };

  services.rsnapshot = {
    enable = true;
    extraConfig = ''
      snapshot_root	/srv/data/lacie/rsnapshot/
      retain	daily	7
      retain	weekly	4
      retain	monthly	12
      verbose		2
      loglevel	3
      backup	/home/		orpheus/
      backup	/etc/		orpheus/
    '';
  };

  systemd.services."rsnapshot@" = {
    after = ["srv-data-lacie.mount"];
    requires = ["srv-data-lacie.mount"];
    description = "rsnapshot (%I) backup";
    serviceConfig = {
      ExecStart = "${pkgs.rsnapshot}/bin/rsnapshot %I";
      Type = "oneshot";
      Nice = 19;
      IOSchedulingClass = "idle";
    };
  };

  systemd.timers."rsnapshot-daily" = {
    description = "rsnapshot daily backup";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "12:00";
      Persistent = true;
      Unit = "rsnapshot@daily.service";
    };
  };

  systemd.timers."rsnapshot-weekly" = {
    description = "rsnapshot weekly backup";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "Monday *-*-* 13:30";
      Persistent = true;
      Unit = "rsnapshot@weekly.service";
    };
  };

  systemd.timers."rsnapshot-monthly" = {
    description = "rsnapshot monthly backup";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "*-*-1 03:30";
      Persistent = true;
      Unit = "rsnapshot@monthly.service";
    };
  };
}
