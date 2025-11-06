{ config, nodes, services, ... }:
let
  primaryUser = config.system.primaryUser;
in
{
  nix.buildMachines = map (nodeName: {
    hostName = nodes.${nodeName}.ip;
    system = nodes.${nodeName}.system;
    maxJobs = 4;
    sshUser = "nix";
    sshKey = "/Users/${primaryUser}/.ssh/id_nix";
  }) services.services.nixBuildMachines;

  nix.distributedBuilds = true;
}
