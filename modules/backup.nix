{ self, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.backup;
in
{
  options.services.backup = {
    enable = mkEnableOption "Enable borg repos";
  };

  config = mkIf cfg.enable {

    services.borgbackup.repos = {
      calliope = {
        authorizedKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDZL7gbthPAl2oquJf/IMpa6VIn/ess6e28NLbepohIT root@calliope"
        ];
        path = "/var/lib/borg/calliope";
      };
    };
    services.openssh.settings.AllowUsers = lib.mkAfter ["borg"];
  };
}
