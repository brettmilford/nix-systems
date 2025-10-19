{
  lib,
  hosts,
  services,
  domain,
}:

let
  # Helper to get data mount for a host
  getDataPath = hostname: hosts.${hostname}.dataPath or "/var/lib";

  # Helper to get backup base path for a host
  getBackupPath = hostname: hosts.${hostname}.backupPath or "${getDataPath hostname}/backup";

  # Helper to resolve service data paths
  resolveServicePath =
    hostname: serviceName:
    let
      svc = services.${serviceName} or null;
      config = if svc != null then svc.config or { } else { };
      dataPath = config.dataPath or serviceName;
    in
      "${getDataPath hostname}/${dataPath}";

  # Get backup repos where this host is a source
  getBackupReposForSource =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
    in
    # Simply check if hostname matches repo name
    if repos ? ${hostname} then { ${hostname} = repos.${hostname}; } else { };

  # Get backup repos where this host is a target
  getBackupReposForTarget =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
    in
    lib.filterAttrs (repoName: repo: builtins.elem hostname (repo.targets or [ ])) repos;

in
{
  inherit domain;

  # ===== Service helpers =====

  # Check if a service runs on a specific host
  hasService =
    hostname: service:
    let
      svc = services.${service} or null;
      hostsList = if svc != null then svc.hosts or [ ] else [ ];
    in
    builtins.elem hostname hostsList;

  # Get primary (first) host for a service
  primaryHost =
    service:
    let
      svc = services.${service} or null;
      hostsList = if svc != null then svc.hosts or [ ] else [ ];
    in
    if hostsList != [ ] then builtins.head hostsList else null;

  # Get all hosts running a service
  hostsFor = service: services.${service}.hosts or [ ];

  # Get all services on a host
  servicesOn =
    hostname:
    builtins.filter (service: builtins.elem hostname (services.${service}.hosts or [ ])) (
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
      host = builtins.head (services.${service}.hosts or [ ]);
      ip = hosts.${host}.ip;
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
      host = hosts.${hostname};
      fqdn = services.${service}.fqdn or null;
    }) (services.${service}.hosts or [ ]);

  # ===== Backup helpers =====

  inherit getBackupReposForSource getBackupReposForTarget;

  # Check if host should perform backups (is a source)
  shouldBackup =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
    in
    repos ? ${hostname};

  # Check if host receives backups (is a target)
  isBackupTarget = hostname: (getBackupReposForTarget hostname) != { };

  # Get backup configuration for a host as a source
  getBackupSourceConfig =
    hostname:
    let
      backupConfig = services.backup.config or { };
      repos = backupConfig.repos or { };
      repoConfig = repos.${hostname} or null;
      sourceHost = hosts.${hostname};
      # Get paths to backup - from host config or default to dataPath
      backupSources = sourceHost.backupSources or [ (getDataPath hostname) ];
    in
    if repoConfig != null then
      {
        ${hostname} = repoConfig // {
          # Source is the hostname
          source = hostname;
          # Paths to backup from the source host
          sources = backupSources;
          # Add target host metadata
          targetHosts = map (target: {
            hostname = target;
            host = hosts.${target};
            repoPath = "${getBackupPath target}/${hostname}";
          }) repoConfig.targets;
        };
      }
    else
      { };

  # Get backup configuration for a host as a target
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
        # Source host metadata
        sourceHost = {
          hostname = repoName;
          host = hosts.${repoName};
          # Paths being backed up from source
          backupSources = hosts.${repoName}.backupSources or [ (getDataPath repoName) ];
        };
      }
    ) repos;

  # Get all backup repos (for debugging/listing)
  getAllBackupRepos = services.backup.config.repos or { };
}
