{
  imports = [
    ../common.nix
    ../libvirt.nix
    ../homebrew.nix
    #../yabai.nix
  ];

  networking = {
    hostName = "thamrys";
    computerName = "thamrys";
    localHostName = "thamrys";
  };
}
