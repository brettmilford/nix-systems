{ config, lib, pkgs, hostname, hosts, serviceMap, ... }:

let
  thisHost = hosts.${hostname};
  isBackupTarget = serviceMap.lib.isBackupTarget hostname;
  backupTargetRepos = serviceMap.lib.getBackupTargetConfig hostname;
in
{
  # Check with nix eval .#nixosConfigurations.calliope.config.services.borgbackup.repos --json | jq
  services.borgbackup.repos = lib.mkIf isBackupTarget (
    lib.mapAttrs' (
      repoName: repoConfig:
      lib.nameValuePair repoName {
        path = repoConfig.repoPath;
        authorizedKeys =
          let
            sourceHost = hosts.${repoConfig.source};
          in
          lib.optional (sourceHost ? backupSshKey) sourceHost.backupSshKey;
      }
    ) backupTargetRepos
  );
  services.openssh.settings.AllowUsers = lib.mkIf isBackupTarget (lib.mkAfter [ "borg" ]);
}
