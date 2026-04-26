{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.services.nfs-server;

  exportLines = concatMapStringsSep "\n" (
    export:
    let
      clientEntries = concatMapStringsSep " " (
        ip: "${ip}(${concatStringsSep "," export.options})"
      ) export.clients;
    in
    "${export.path} ${clientEntries}"
  ) cfg.exports;
in
{
  options.services.nfs-server = {
    enable = mkEnableOption "NFS server";
    exports = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            path = mkOption {
              type = types.str;
              description = "Path to export";
            };
            clients = mkOption {
              type = types.listOf types.str;
              description = "Client IP addresses allowed to mount";
            };
            options = mkOption {
              type = types.listOf types.str;
              default = [
                "ro"
                "no_subtree_check"
                "no_root_squash"
              ];
              description = "NFS export options";
            };
          };
        }
      );
      default = [ ];
      description = "NFS exports";
    };
  };

  config = mkIf cfg.enable {
    services.nfs.server = {
      enable = true;
      lockdPort = 4001;
      mountdPort = 4002;
      statdPort = 4000;
      exports = exportLines;
    };

    networking.firewall = {
      allowedTCPPorts = [
        2049
        111
        4000
        4001
        4002
      ];
      allowedUDPPorts = [
        2049
        111
        4000
        4001
        4002
      ];
    };
  };
}
