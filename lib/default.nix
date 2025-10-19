{ lib }:

lib.fix (self: {
  # Validate that all referenced hosts exist
  validateHostReferences = hosts: references:
    let
      allHostnames = lib.attrNames hosts;
      invalidHosts = lib.filter (h: !(lib.elem h allHostnames)) references;
    in
      if invalidHosts != [] then
        throw "Invalid host references: ${lib.toString invalidHosts}"
      else
        true;

  # Validate service configuration
  validateServices = hosts: services:
    let
      allServiceHosts = lib.unique (lib.flatten (
        lib.mapAttrsToList (name: svc:
          if lib.isList svc then svc
          else svc.hosts or []
        ) services
      ));
    in
      self.validateHostReferences hosts allServiceHosts;

  # Validate backup topology
  validateBackupSets = hosts: backupRepos:
    let
      allBackupTargets = lib.unique (lib.flatten (
        lib.mapAttrsToList (repoName: repo:
          repo.targets or []
        ) backupRepos
      ));
      allBackupSources = lib.unique (lib.flatten (
        lib.mapAttrsToList (repoName: repo:
          repo.sources or []
        ) backupRepos
      ));
    in
      self.validateHostReferences hosts (allBackupTargets ++ allBackupSources);
})
