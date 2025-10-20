{
  opnsense = {
    hostname = "opnsense";
    ip = "192.168.1.1";
    system = "amd64-freebsd";
  };

  orpheus = {
    hostname = "orpheus";
    ip = "192.168.10.31";
    domain = "orpheus.internal";
    system = "x86_64-linux";
    dataPath = "/srv";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHSeI7btghOWNVXXYq7A35Z2th8+w431soOMYfDL0YLr borg@orpheus";
  };

  eurydice = {
    hostname = "eurydice";
    ip = "192.168.1.2";
    domain = "eurydice.internal";
    system = "x86_64-linux";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOh68LdB9GCJ0M8UW+gfr9nFnftsr7GdUc/O7T2ZyLY5 borg@eurydice";
  };

  calliope = {
    hostname = "calliope";
    ip = "172.22.70.58";
    domain = "calliope.internal";
    system = "aarch64-linux";
    dataPath = "/srv/data";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDZL7gbthPAl2oquJf/IMpa6VIn/ess6e28NLbepohIT borg@calliope";
  };

  terpsichore = {
    hostname = "terpsichore";
    ip = "192.168.1.16";
    domain = "terpsichore.internal";
    system = "x86_64-linux";
    dataPath = "/srv";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMbevo0T1bXdD4uD8x/33IhXfP2alhpnswYn7DDdeWgI borg@terpsichore";
  };

  thamrys = {
    hostname = "thamrys";
    ip = "192.168.10.11";
    domain = "thamrys.internal";
    system = "aarch64-darwin";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJvYLvNsnBmVqrC9IUZIEeaGElcUcHR2w1yOQXaOC9LV borg@thamrys";
  };
}
