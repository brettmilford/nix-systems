{ config, pkgs, ... }:
{
  services.postgresql = {
     enable = true;
     ensureDatabases = [ config.services.nextcloud.config.dbname ];
     ensureUsers = [{
       name = config.services.nextcloud.config.dbuser;
       ensurePermissions = { "DATABASE ${config.services.nextcloud.config.dbname}" = "ALL PRIVILEGES"; };
     }];
  };
}
