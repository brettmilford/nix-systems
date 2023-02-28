{
  imports = [
    ../common.nix
    #../libvirt.nix
    ../homebrew.nix
    ./node-exporter.nix
    ../yabai.nix
  ];

  networking = {
    hostName = "thamrys";
    computerName = "thamrys";
    localHostName = "thamrys";
  };
}
