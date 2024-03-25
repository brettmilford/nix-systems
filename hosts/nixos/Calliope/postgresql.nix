{
  config,
  pkgs,
  ...
}: {
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
    ensureDatabases = [config.services.nextcloud.config.dbname];
    ensureUsers = [
      {
        name = config.services.nextcloud.config.dbuser;
        ensurePermissions = {"DATABASE ${config.services.nextcloud.config.dbname}" = "ALL PRIVILEGES";};
      }
    ];
  };
}
