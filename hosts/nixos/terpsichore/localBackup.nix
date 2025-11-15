{
  config,
  lib,
  pkgs,
  hostname,
  ...
}:
let
  fs = "/mnt/toshiba";
in
{
  fileSystems."${fs}" = {
    device = "/dev/disk/by-uuid/42FAB103FAB0F3EF";
    fsType = "ntfs-3g";
    options = [
      "rw"
      "umask=022"
      "nosuid"
      "nodev"
      "nofail"
      "noauto"
      "x-systemd.automount"
      "x-systemd.idle-timeout=10m"
    ];
  };

  services.borgbackup.jobs.localBackup = {
    paths = [
      "/srv"
      "/var/lib"
    ];
    repo = "${fs}/borg";
    doInit = true;
    encryption.mode = "none";
    compression = "auto,zstd";
    # Equivalent to rsnapshot excludes
    patterns = [
      "+ /var/lib/postgresql/backups"
      "- /var/lib/acme"
      "- /var/lib/containers"
      "- /var/lib/fail2ban"
      "- /var/lib/ipfs"
      "- /var/lib/redis-immich"
      "- /var/lib/redis-nextcloud"
      "- /var/lib/redis-paperless"
      "- /var/lib/systemd"
      "- /var/lib/postgresql"
      "+ /var/lib/postgresql/backups"
    ];

    prune.keep = {
      daily = 7;
      weekly = 4;
      monthly = 12;
    };

    startAt = "01:00";
  };

  systemd.services.borgbackup-job-localBackup = {
    after = [ "mnt-toshiba.mount" ];
    requires = [ "mnt-toshiba.mount" ];
  };
}
