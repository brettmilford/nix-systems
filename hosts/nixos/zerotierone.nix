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
    nameservers = [ "172.22.0.1" ];
    search = [ "cirriform.au" ];
  };
}
