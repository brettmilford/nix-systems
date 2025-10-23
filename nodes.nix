{ self }:
{
  orpheus = {
    ip = "192.168.10.31";
    domain = "internal";
    system = "x86_64-linux";
    dataPath = "/srv";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHSeI7btghOWNVXXYq7A35Z2th8+w431soOMYfDL0YLr borg@orpheus";
    extraModules = [
      self.nixosModules.users
    ];
  };

  eurydice = {
    ip = "192.168.1.2";
    domain = "internal";
    system = "x86_64-linux";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOh68LdB9GCJ0M8UW+gfr9nFnftsr7GdUc/O7T2ZyLY5 borg@eurydice";
    extraModules = [
      self.nixosModules.secureBoot
    ];
  };

  calliope = {
    ip = "172.22.70.58";
    domain = "internal";
    system = "aarch64-linux";
    dataPath = "/srv/data";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDZL7gbthPAl2oquJf/IMpa6VIn/ess6e28NLbepohIT borg@calliope";
  };

  terpsichore = {
    ip = "192.168.1.16";
    domain = "internal";
    system = "x86_64-linux";
    dataPath = "/srv";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMbevo0T1bXdD4uD8x/33IhXfP2alhpnswYn7DDdeWgI borg@terpsichore";
    extraModules = [
      self.nixosModules.secureBoot
    ];
  };

  thamrys = {
    ip = "192.168.10.11";
    domain = "internal";
    system = "aarch64-darwin";
    backupSshKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJvYLvNsnBmVqrC9IUZIEeaGElcUcHR2w1yOQXaOC9LV borg@thamrys";
  };
}
