{ config, serviceMap, hosts, ... }:
let
  primaryUser = config.system.primaryUser;
in
{
  nix.buildMachines = map (hostname: {
    hostName = hosts.${hostname}.ip;
    system = hosts.${hostname}.system;
    maxJobs = 4;
    sshUser = "nix";
    sshKey = "/Users/${primaryUser}/.ssh/id_ed25519";
  }) serviceMap.services.nixBuildMachines;

  nix.distributedBuilds = true;
}
