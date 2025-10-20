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
          }
        ) repoConfig.targetHosts
      )
    ) backupSourceRepos
  );
}
