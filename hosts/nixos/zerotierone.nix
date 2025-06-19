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
}
