{
  config,
  lib,
  pkgs,
  self,
  hostname,
  nodes,
  services,
  ...
}:
let
  shouldBackup = services.lib.shouldBackup hostname;
  backupSourceRepos = services.lib.getBackupSourceConfig hostname;
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

            patterns = [
              "- /var/lib/qemu"
              "+ /var/lib/qemu/snapshots/opnsense"
              "+ /var/lib/qemu/images/opnsense"
              "- /var/lib/acme"
              "- /var/lib/containers"
              "- /var/lib/fail2ban"
              "- /var/lib/systemd"
              "- /var/lib/hass/home-assistant_v2.db"
              "- /var/lib/hass/home-assistant_v2.db-wal"
              "- /var/lib/hass/home-assistant_v2.db-shm"
              "- /var/lib/loki"
              "- /var/lib/prometheus2"
            ];

            repo = "borg@${target.node.ip}:${target.repoPath}";
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

            readWritePaths = [ "/var/lib/qemu/snapshots" ];

            preHook = ''
              echo "Creating OPNsense VM snapshot before backup..."
              SNAPSHOT_NAME="backup_$(date +%Y%m%d_%H%M%S)"
              ${pkgs.qemu-snapshot}/bin/qemu-snapshot opnsense create "$SNAPSHOT_NAME" || echo "Warning: VM snapshot failed"
            '';
          }
        ) repoConfig.targetNodes
      )
    ) backupSourceRepos
  );
}
