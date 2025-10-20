{
  config,
  lib,
  pkgs,
  self,
  hostname,
  hosts,
  serviceMap,
  users,
  ...
}:
let
  thisHost = hosts.${hostname};
  shouldBackup = serviceMap.lib.shouldBackup hostname;
  backupSourceRepos = serviceMap.lib.getBackupSourceConfig hostname;
  backupRWPath = "/var/lib/postgresql/backups/";
in
{
  age.secrets.borg-ssh-key.file = "${self}/secrets/borg-${hostname}-ssh-key.age";
  services.borgbackup.jobs = lib.mkIf shouldBackup (
    lib.concatMapAttrs (
      repoName: repoConfig:
      lib.listToAttrs (
        lib.imap1 (
          idx: target:
          lib.nameValuePair "${repoName}-${target.hostname}" {
            paths = [ "/var/lib" ];

            exclude = [
              "/var/lib/acme"
              "/var/lib/containers"
              "/var/lib/fail2ban"
              "/var/lib/ipfs"
              "/var/lib/redis-immich"
              "/var/lib/redis-nextcloud"
              "/var/lib/redis-paperless"
              "/var/lib/systemd"
            ];

            repo = "borg@${target.host.ip}:${target.repoPath}";
            doInit = true;
            environment.BORG_RSH = "ssh -i ${config.age.secrets.borg-ssh-key.path}";

            encryption.mode = "none";

            compression = "auto,lzma";

            # Stagger start times by target
            startAt = "0${toString (2 + idx)}:00";

            prune.keep = {
              within = "1d";
              daily = 7;
              weekly = 4;
              monthly = 6;
            };

            readWritePaths = [ "${backupRWPath}" ];
            preHook = ''
              echo "Starting PostgreSQL backup..."

              # Backup global objects (roles, tablespaces, etc.)
              echo "Backing up PostgreSQL globals..."
              ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/pg_dumpall --globals-only > ${backupRWPath}/postgres_globals.sql
              ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/pg_dumpall > ${backupRWPath}/postgres_all.sql

              # Get list of databases and backup each individually
              echo "Backing up individual databases..."
              for db in $(${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/psql -t -c "select datname from pg_database where not datistemplate" | ${pkgs.gnugrep}/bin/grep '\S' | ${pkgs.gawk}/bin/awk '{$1=$1};1'); do
                echo "  Backing up database: $db"
                ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql}/bin/pg_dump --create --format=custom "$db" > "${backupRWPath}/$db.pgdump"
              done

              echo "PostgreSQL backup completed"
            '';

          }
        ) repoConfig.targetHosts
      )
    ) backupSourceRepos
  );
}
