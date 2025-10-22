{ config, lib, pkgs, hostname, nodes, services, ... }:

let
  thisNode = nodes.${hostname};
  isBackupTarget = services.lib.isBackupTarget hostname;
  backupTargetRepos = services.lib.getBackupTargetConfig hostname;
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
            sourceNode = nodes.${repoConfig.source};
          in
          lib.optional (sourceNode ? backupSshKey) sourceNode.backupSshKey;
      }
    ) backupTargetRepos
  );
  services.openssh.settings.AllowUsers = lib.mkIf isBackupTarget (lib.mkAfter [ "borg" ]);
}
