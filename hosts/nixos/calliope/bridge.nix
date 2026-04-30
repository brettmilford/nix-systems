{ pkgs, ... }:

{
  services.protonmail-bridge = {
    enable = true;
    path = [ pkgs.pass ];
    logLevel = "info";
  };
}
