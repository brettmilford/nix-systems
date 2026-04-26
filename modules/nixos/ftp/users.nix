{ config, ... }:
let
  cfg = config.services.ftp;
in
{
  users.users.reolink = {
    isSystemUser = true;
    group = "reolink";
    home = "${cfg.dataPath}/reolink";
    createHome = true;
    shell = "/sbin/nologin";
  };
  users.groups.reolink = { };
}
