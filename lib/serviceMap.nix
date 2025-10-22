{
  lib,
  nodes,
  services
}:

let
  # Helper to get data mount for a node
  getDataPath = hostname: nodes.${hostname}.dataPath or "/var/lib";

  # Helper to get backup base path for a node
  getBackupPath = hostname: nodes.${hostname}.backupPath or "${getDataPath hostname}/backup";

  # Helper to resolve service data paths
  resolveServicePath =
    hostname: serviceName:
    let
      svc = services.${serviceName} or null;
      config = if svc != null then svc.config or { } else { };
      dataPath = config.dataPath or serviceName;
    in
      "${getDataPath hostname}/${dataPath}";

  # Get backup repos where this node is a source
  getBackupReposForSource =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
    in
    # Simply check if hostname matches repo name
    if repos ? ${hostname} then { ${hostname} = repos.${hostname}; } else { };

  # Get backup repos where this node is a target
  getBackupReposForTarget =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
    in
    lib.filterAttrs (repoName: repo: builtins.elem hostname (repo.targets or [ ])) repos;

  validateHostReferences = hosts: references:
    let
      allHostnames = lib.attrNames hosts;
      invalidHosts = lib.filter (h: !(lib.elem h allHostnames)) references;
    in
      if invalidHosts != [] then
        throw "Invalid host references: ${lib.toString invalidHosts}"
      else
        true;
in
{
  # ===== Service helpers =====

  # Check if a service runs on a specific node
  hasService =
    hostname: service:
    let
      svc = services.${service} or null;
      nodesList = if svc != null then svc.hosts or [ ] else [ ];
    in
    builtins.elem hostname nodesList;

  # Get primary (first) node for a service
  primaryNode =
    service:
    let
      svc = services.${service} or null;
      nodesList = if svc != null then svc.hosts or [ ] else [ ];
    in
    if nodesList != [ ] then builtins.head nodesList else null;

  # Get all nodes running a service
  nodesFor = service: services.${service}.hosts or [ ];

  # Get all services on a host
  servicesOn =
    hostname:
    builtins.filter (service: builtins.elem hostname (services.${service}.nodes or [ ])) (
      builtins.attrNames services
    );

  # Get service FQDN
  getServiceFQDN = service: services.${service}.fqdn or null;

  # Get service config
  getServiceConfig = service: services.${service}.config or { };

  # Get service data path (resolves dataPath)
  getServiceDataPath = hostname: serviceName: resolveServicePath hostname serviceName;

  # Get service URL
  getServiceUrl =
    service:
    let
      fqdn = services.${service}.fqdn or null;
      node = builtins.head (services.${service}.nodes or [ ]);
      ip = nodes.${node}.ip;
      config = services.${service}.config or { };
      port = config.port or null;
      baseUrl = if fqdn != null then "https://${fqdn}" else "http://${ip}";
    in
    if port != null then "${baseUrl}:${toString port}" else baseUrl;

  # Get all service endpoints with metadata
  getServiceEndpoints =
    service:
    builtins.map (hostname: {
      inherit hostname;
      node = nodes.${hostname};
      fqdn = services.${service}.fqdn or null;
    }) (services.${service}.nodes or [ ]);

  # ===== Backup helpers =====

  inherit getBackupReposForSource getBackupReposForTarget;

  # Check if node should perform backups (is a source)
  shouldBackup =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
    in
    repos ? ${hostname};

  # Check if node receives backups (is a target)
  isBackupTarget = hostname: (getBackupReposForTarget hostname) != { };

  # Get backup configuration for a node as a source
  getBackupSourceConfig =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
      repoConfig = repos.${hostname} or null;
      sourceNode = nodes.${hostname};
      # Get paths to backup - from node config or default to dataPath
      backupSources = sourceNode.backupSources or [ (getDataPath hostname) ];
    in
    if repoConfig != null then
      {
        ${hostname} = repoConfig // {
          # Source is the hostname
          source = hostname;
          # Paths to backup from the source host
          sources = backupSources;
          # Add target node metadata
          targetNodes = map (target: {
            hostname = target;
            node = nodes.${target};
            repoPath = "${getBackupPath target}/${hostname}";
          }) repoConfig.targets;
        };
      }
    else
      { };

  # Get backup configuration for a node as a target
  getBackupTargetConfig =
    hostname:
    let
      repos = getBackupReposForTarget hostname;
      backupPath = getBackupPath hostname;
    in
    builtins.mapAttrs (
      repoName: repo:
      repo
      // {
        # Source hostname is the repo name
        source = repoName;
        # Repo path on this target
        repoPath = "${backupPath}/${repoName}";
        # Source node metadata
        sourceNode = {
          hostname = repoName;
          node = nodes.${repoName};
          # Paths being backed up from source
          backupSources = nodes.${repoName}.backupSources or [ (getDataPath repoName) ];
        };
      }
    ) repos;

  # Get all backup repos (for debugging/listing)
  getAllBackupRepos = services.backup.config.repos or { };

  # ===== Validation helpers =====

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
      validateHostReferences hosts allServiceHosts;

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
      validateHostReferences hosts (allBackupTargets ++ allBackupSources);
}
