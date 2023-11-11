{ config, lib, pkgs, ... }:

{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "zerotierone"
  ];

  services.zerotierone = {
    enable = true;
    joinNetworks = [ "ebe7fbd44553bc18" ];
  };
}
