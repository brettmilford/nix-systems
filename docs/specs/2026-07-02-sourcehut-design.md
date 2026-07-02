# SourceHut on NixOS via Incus Containers

## Overview

Deploy SourceHut services on terpsichore (NixOS x86_64-linux) using Incus system containers running Alpine Linux. PostgreSQL, Redis, and nginx run natively on the NixOS host. Each SourceHut service runs in its own isolated Alpine container.

## Architecture

```
terpsichore (NixOS host, x86_64-linux, /srv dataPath)
┌─────────────────────────────────────────────────────────┐
│  nginx (gateway.nix) — TLS termination                  │
│  ┌──────────────────────────────────────────────────┐   │
│  │ meta.cirriform.au  → 10.0.100.10:5000/5100       │   │
│  │ git.cirriform.au   → 10.0.100.11:5001/5101       │   │
│  │ todo.cirriform.au  → 10.0.100.12:5002/5102       │   │
│  │ hub.cirriform.au   → 10.0.100.13:5003/5103       │   │
│  │ man.cirriform.au   → 10.0.100.14:5004/5104       │   │
│  │ pages.cirriform.au → 10.0.100.15:5005/5105       │   │
│  │ builds.cirriform.au→ 10.0.100.16:5006/5106       │   │
│  └──────────────────────────────────────────────────┘   │
│                                                         │
│  PostgreSQL (host, native)   Redis (host, native)       │
│  ├── meta_srht               └── shared instance        │
│  ├── git_srht                                           │
│  ├── todo_srht                                          │
│  ├── hub_srht                                           │
│  ├── man_srht                                           │
│  ├── pages_srht                                         │
│  └── builds_srht                                        │
│                                                         │
│  Incus (incusbr0: 10.0.100.1/24)                       │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐   │
│  │srht-meta │ │srht-git  │ │srht-todo │ │srht-hub  │   │
│  │ (Alpine) │ │ (Alpine) │ │ (Alpine) │ │ (Alpine) │   │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘   │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐               │
│  │srht-man  │ │srht-pages│ │srht-build│               │
│  │ (Alpine) │ │ (Alpine) │ │ (Alpine) │               │
│  └──────────┘ └──────────┘ └──────────┘               │
└─────────────────────────────────────────────────────────┘
```

### Services

| Service | Container | FQDN | Web Port | API Port | Description |
|---------|-----------|------|----------|----------|-------------|
| meta.sr.ht | srht-meta | meta.cirriform.au | 5000 | 5100 | Account management, auth (required) |
| git.sr.ht | srht-git | git.cirriform.au | 5001 | 5101 | Git repository hosting |
| todo.sr.ht | srht-todo | todo.cirriform.au | 5002 | 5102 | Bug tracking |
| hub.sr.ht | srht-hub | hub.cirriform.au | 5003 | 5103 | Project management |
| man.sr.ht | srht-man | man.cirriform.au | 5004 | 5104 | Wiki / documentation |
| pages.sr.ht | srht-pages | pages.cirriform.au | 5005 | 5105 | Static site hosting |
| builds.sr.ht | srht-builds | builds.cirriform.au | 5006 | 5106 | CI/CD (requires KVM) |

Each service runs two daemons: a Python web frontend and a Go GraphQL API daemon. nginx routes `/` to the web frontend and `/query` (or `/api`) to the API daemon.

## Module Structure

```
modules/nixos/
├── incus.nix            # Reusable Incus infrastructure module (like gateway.nix)
└── sourcehut/
    ├── default.nix      # Main entry: options, imports incus.nix, PostgreSQL, Redis
    ├── containers.nix   # Declarative container provisioning and lifecycle
    ├── meta.nix         # meta.sr.ht container and service config
    ├── git.nix          # git.sr.ht container and service config
    ├── todo.nix         # todo.sr.ht container and service config
    ├── hub.nix          # hub.sr.ht container and service config
    ├── man.nix          # man.sr.ht container and service config
    ├── pages.nix        # pages.sr.ht container and service config
    └── builds.nix       # builds.sr.ht container and service config
```

### incus.nix — Reusable Infrastructure Module

Similar to `gateway.nix`, this is a standalone module that any service can import to get Incus container support:

```nix
options.services.incus = {
  enable = mkEnableOption "Incus container and VM management";

  dataPath = mkOption {
    type = types.str;
    default = "/var/lib/incus";
    description = "Directory for Incus storage pools";
  };

  bridgeNetwork = mkOption {
    type = types.str;
    default = "10.0.100.1/24";
    description = "CIDR for the Incus bridge network";
  };

  defaultProfileDiskSize = mkOption {
    type = types.str;
    default = "10GiB";
    description = "Default disk size for the default profile";
  };
};
```

When enabled, it configures:
- Incus daemon via `virtualisation.incus`
- Bridge network (`incusbr0`)
- Directory-based storage pool at `${dataPath}/storage-pools/default`
- Default profile with network and root disk device

SourceHut imports this module and sets `dataPath` to its service data path.

### Options (default.nix)

```nix
options.services.sourcehut = {
  enable = mkEnableOption "SourceHut";

  domain = mkOption {
    type = types.str;
    default = "cirriform.au";
    description = "Base domain for SourceHut services";
  };

  services = mkOption {
    type = types.listOf (types.enum [
      "meta" "git" "todo" "hub" "man" "pages" "builds"
    ]);
    default = [ "meta" "git" "todo" "hub" "man" "pages" "builds" ];
    description = "Which SourceHut services to enable";
  };

  dataPath = mkOption {
    type = types.str;
    default = "/srv/data/sourcehut";
    description = "Base data directory for SourceHut";
  };

  secretPaths = mkOption {
    type = types.attrs;
    description = "Agenix secret file paths (auto-populated by framework)";
  };
};
```

### Integration with Service Framework

**services.nix:**

```nix
sourcehut = {
  hosts = [ "terpsichore" ];
  config = {
    dataPath = "sourcehut";
  };
};
```

**serviceModules.nix:**

```nix
sourcehut = {
  modules = [ "${mod}/sourcehut" ];
  getOptions = hostname: {
    services.sourcehut = {
      enable = true;
      dataPath = services.lib.getServiceDataPath hostname "sourcehut";
    };
  };
  getSecrets = hostname: {
    "srht-meta-config" = {
      file = "${secrets}/srht-meta-config.age";
      owner = "root"; group = "root";
    };
    "srht-git-config" = {
      file = "${secrets}/srht-git-config.age";
      owner = "root"; group = "root";
    };
    "srht-todo-config" = {
      file = "${secrets}/srht-todo-config.age";
      owner = "root"; group = "root";
    };
    "srht-hub-config" = {
      file = "${secrets}/srht-hub-config.age";
      owner = "root"; group = "root";
    };
    "srht-man-config" = {
      file = "${secrets}/srht-man-config.age";
      owner = "root"; group = "root";
    };
    "srht-pages-config" = {
      file = "${secrets}/srht-pages-config.age";
      owner = "root"; group = "root";
    };
    "srht-builds-config" = {
      file = "${secrets}/srht-builds-config.age";
      owner = "root"; group = "root";
    };
    "srht-shared-secret" = {
      file = "${secrets}/srht-shared-secret.age";
      owner = "root"; group = "root";
    };
  };
};
```

## Host-Side Infrastructure (NixOS)

### Incus Configuration

SourceHut's `default.nix` imports `../incus.nix` and enables it:

```nix
imports = [ ../incus.nix ];

config = mkIf cfg.enable {
  services.incus = {
    enable = true;
    dataPath = "${cfg.dataPath}/containers";
  };
  # ...
};
```

The `incus.nix` module handles all Incus setup (bridge network, storage pool, default profile) using the provided `dataPath`.

### PostgreSQL

One database and user per service, managed by NixOS:

```nix
services.postgresql = {
  enable = true;
  ensureDatabases = [
    "meta_srht" "git_srht" "todo_srht"
    "hub_srht" "man_srht" "pages_srht" "builds_srht"
  ];
  ensureUsers = [
    { name = "meta_srht";   ensureDBOwnership = true; }
    { name = "git_srht";    ensureDBOwnership = true; }
    { name = "todo_srht";   ensureDBOwnership = true; }
    { name = "hub_srht";    ensureDBOwnership = true; }
    { name = "man_srht";    ensureDBOwnership = true; }
    { name = "pages_srht";  ensureDBOwnership = true; }
    { name = "builds_srht"; ensureDBOwnership = true; }
  ];
};
```

PostgreSQL must listen on the Incus bridge interface (`10.0.100.1`) so containers can connect. `pg_hba.conf` must allow connections from the `10.0.100.0/24` subnet.

### Database Initialization

Each service's database must be initialized after container creation. The `sourcehut-migrate` tool (installed inside each Alpine container) handles this:

```bash
incus exec srht-meta -- sourcehut-migrate meta.sr.ht init
incus exec srht-git  -- sourcehut-migrate git.sr.ht init
# ... one per service
```

This is run once during initial provisioning. Subsequent migrations are handled automatically on package upgrades if `migrate-on-upgrade = yes` is set in config.ini.

### Redis

```nix
services.redis.servers.sourcehut = {
  enable = true;
  bind = "10.0.100.1";
  port = 6379;
};
```

### nginx Reverse Proxy

Each service gets a virtual host via gateway.nix with dual-port routing:

```nix
services.gateway.enable = true;

services.nginx.virtualHosts."meta.cirriform.au" = {
  locations."/" = {
    proxyPass = "http://10.0.100.10:5000";
  };
  locations."/query" = {
    proxyPass = "http://10.0.100.10:5100";
  };
};
# ... repeated for each service
```

## Container Lifecycle Management

### Container Provisioning

Each container is managed by a systemd activation script:

```nix
systemd.services."srht-container-meta" = {
  description = "SourceHut meta.sr.ht container";
  wantedBy = [ "multi-user.target" ];
  after = [ "incus.service" "incus-preseed.service" ];
  requires = [ "incus.service" ];
  path = [ config.virtualisation.incus.package ];

  script = ''
    if ! incus info srht-meta &>/dev/null; then
      incus launch images:alpine/edge srht-meta
      incus config set srht-meta boot.autostart true

      incus exec srht-meta -- sh -c '
        setup-apkrepos -c -e
        wget -qO /etc/apk/keys/alpine@sr.ht.rsa.pub \
          https://mirror.sr.ht/alpine/sr.ht.rsa.pub
        echo "https://mirror.sr.ht/alpine/edge/sr.ht" \
          >> /etc/apk/repositories
        apk update
        apk add meta.sr.ht
        rc-update add meta.sr.ht-api default
        rc-update add meta.sr.ht default
      '
    fi

    incus file push \
      ${config.age.secrets.srht-meta-config.path} \
      srht-meta/etc/sr.ht/config.ini

    incus exec srht-meta -- rc-service meta.sr.ht-api restart
    incus exec srht-meta -- rc-service meta.sr.ht restart
    incus start srht-meta || true
  '';
};
```

### Static IP Assignment

Each container gets a static IP via Incus NIC configuration:

```nix
# After container creation:
incus config device set srht-meta eth0 ipv4.address=10.0.100.10
incus config device set srht-git  eth0 ipv4.address=10.0.100.11
incus config device set srht-todo eth0 ipv4.address=10.0.100.12
incus config device set srht-hub  eth0 ipv4.address=10.0.100.13
incus config device set srht-man  eth0 ipv4.address=10.0.100.14
incus config device set srht-pages eth0 ipv4.address=10.0.100.15
incus config device set srht-builds eth0 ipv4.address=10.0.100.16
```

### Container Networking

Containers reach host services via the bridge gateway IP:
- PostgreSQL: `10.0.100.1:5432`
- Redis: `10.0.100.1:6379`

Host nginx reaches containers via their static IPs on the `incusbr0` bridge.

## Secrets and Configuration

### config.ini Structure

Each SourceHut service has a config.ini with a shared `[sr.ht]` section and service-specific sections:

```ini
[sr.ht]
origin = meta.cirriform.au
connection-string = postgresql://meta_srht:<password>@10.0.100.1/meta_srht
redis-host = 10.0.100.1
private-key = <shared-private-key>

[meta.sr.ht]
...service-specific configuration...
```

### Agenix Secrets

Each service's config.ini is stored as an agenix secret on the host and pushed into the container at activation time. A shared secret file contains the `[sr.ht]` common section (origin, private-key, redis-host) that is identical across services.

PostgreSQL passwords are embedded in the connection-string within each config.ini secret.

### Secret Files

```
secrets/
├── srht-meta-config.age
├── srht-git-config.age
├── srht-todo-config.age
├── srht-hub-config.age
├── srht-man-config.age
├── srht-pages-config.age
├── srht-builds-config.age
└── srht-shared-secret.age
```

## builds.sr.ht Special Considerations

builds.sr.ht runs build workers that execute tasks inside VMs with KVM passthrough.

### Requirements

- KVM device passthrough to the container
- Nested virtualization enabled on the container
- Host must have KVM support (terpsichore is bare-metal x86_64, so this is available)

### Container Configuration

```nix
incus config set srht-builds security.nesting=true
incus config device add srht-builds kvm unix-char path=/dev/kvm
```

### Phased Rollout

If KVM nesting inside Incus containers proves problematic, builds.sr.ht can be deferred to a later phase. The other 6 services function independently without it.

## Verification

### Build Validation

```bash
nix flake check
nix build '.#nixosConfigurations.terpsichore.config.system.build.toplevel'
```

### Runtime Validation

After deployment to terpsichore:

1. `incus list` — all 7 containers running with correct IPs
2. `curl https://meta.cirriform.au` — meta.sr.ht web UI responds
3. Register an account and verify OAuth flow
4. `curl https://git.cirriform.au` — git.sr.ht responds
5. Create a test repository and clone it
6. Verify todo, hub, man, pages services authenticate via meta.sr.ht
7. (Optional) Submit a build to builds.sr.ht and verify execution
