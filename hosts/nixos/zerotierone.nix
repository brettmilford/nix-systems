{
  config,
  lib,
  pkgs,
  ...
}: {
  services.zerotierone = {
    enable = true;
    joinNetworks = ["ebe7fbd44553bc18"];
  };

  networking = lib.mkIf config.services.zerotierone.enable {
    nameservers = [ "172.16.1.1" ];
    search = [ "cirriform.au" ];
  };
}
