{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services."git-server";
in
{
  options.services."git-server" = {
    enable = mkEnableOption "git SSH server";

    dataPath = mkOption {
      type = types.str;
      description = "Directory where git repositories are stored";
    };

    authorizedKeys = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "SSH public keys authorized to access git repositories";
    };

  };

  config = mkIf cfg.enable {
    users.users.git = {
      isSystemUser = true;
      group = "git";
      home = cfg.dataPath;
      createHome = true;
      shell = "${pkgs.git}/bin/git-shell";
      openssh.authorizedKeys.keys = cfg.authorizedKeys;
    };
    users.groups.git = { };
    services.openssh.settings.AllowUsers = lib.mkAfter [ "git" ];
  };
}
