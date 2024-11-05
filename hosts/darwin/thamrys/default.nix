{
  imports = [
    ../common.nix
    ../libvirt.nix
    ../homebrew.nix
    #../yabai.nix
    ../podman.nix
  ];

  networking = {
    hostName = "thamrys";
    computerName = "thamrys";
    localHostName = "thamrys";
  };

  services.podman.enable = true;
}
