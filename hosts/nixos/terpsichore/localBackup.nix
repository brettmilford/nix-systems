{
  config,
  lib,
  pkgs,
  hostname,
  ...
}:
let
  fs = "/mnt/toshiba";
  backupRWPath = "/var/lib/postgresql/backups";
  pg = config.services.postgresql.package;
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

  systemd.tmpfiles.rules = [
    "d ${backupRWPath} 0750 postgres postgres -"
  ];

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

    readWritePaths = [ backupRWPath ];

    preHook = ''
      echo "Starting PostgreSQL backup..."
      ${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/pg_dumpall --globals-only > ${backupRWPath}/postgres_globals.sql
      ${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/pg_dumpall > ${backupRWPath}/postgres_all.sql
      for db in $(${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/psql -t -c "select datname from pg_database where not datistemplate" | ${pkgs.gnugrep}/bin/grep '\S' | ${pkgs.gawk}/bin/awk '{$1=$1};1'); do
        echo "  Backing up database: $db"
        ${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/pg_dump --create --format=custom "$db" > "${backupRWPath}/$db.pgdump"
      done
      echo "PostgreSQL backup completed"
    '';

    startAt = "01:00";
  };

  systemd.services.borgbackup-job-localBackup = {
    after = [ "mnt-toshiba.mount" ];
    requires = [ "mnt-toshiba.mount" ];
  };
}
