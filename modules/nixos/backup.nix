{
  self,
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.backup;
in
{
  options.services.backup = {
    enable = mkEnableOption "Enable borg repos";
    repos = lib.mkOption {
      type = lib.types.attrs;
      description = "Attrs of repos";
    };
  };

  config = mkIf cfg.enable {

    # Check with nix eval .#nixosConfigurations.terpsichore.config.services.borgbackup.repos --json | jq
    services.borgbackup.repos = (
      lib.mapAttrs' (
        repoName: repoConfig:
        lib.nameValuePair repoName {
          path = repoConfig.repoPath;
          authorizedKeys = repoConfig.authorizedKeys or [];
        }
      ) cfg.repos
    );
    services.openssh.settings.AllowUsers = lib.mkAfter [ "borg" ];
  };
}
