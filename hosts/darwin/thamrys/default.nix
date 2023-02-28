{
  imports = [
    ../common.nix
    ../libvirt.nix
    ../homebrew.nix
    ../yabai.nix
    ./node-exporter.nix
  ];

  networking = {
    hostName = "thamrys";
    computerName = "thamrys";
    localHostName = "thamrys";
  };
}
