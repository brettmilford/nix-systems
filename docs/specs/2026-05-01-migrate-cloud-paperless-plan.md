# Migrate Cloud & Paperless to Terpsichore — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move cloud (Nextcloud) and paperless-ngx services from calliope to terpsichore by updating Nix config, fixing hard-coded paths, and cleaning up calliope.

**Architecture:** Update `services.nix` host assignments, fix `modules/nixos/cloud.nix` to derive all paths and domains from config options, remove `bomftp.nix`, clean up calliope's PostgreSQL and backup config. Both services move together preserving their same-host coupling.

**Tech Stack:** Nix, NixOS modules, agenix, PostgreSQL, Nextcloud, Paperless-ngx

**Spec:** `docs/specs/2026-05-01-migrate-cloud-paperless-to-terpsichore.md`

---

### File Map

| File | Action | Purpose |
|---|---|---|
| `modules/nixos/cloud.nix` | Modify | Fix hard-coded domains and paths |
| `services.nix` | Modify | Reassign cloud + paperless to terpsichore |
| `hosts/nixos/calliope/default.nix` | Modify | Remove bomftp import, update pg config |
| `hosts/nixos/calliope/bomftp.nix` | Delete | No longer needed |
| `hosts/nixos/calliope/backup.nix` | Modify | Use dynamic pg package, remove stale excludes |

---

### Task 1: Fix hard-coded domains in `modules/nixos/cloud.nix`

**Files:**
- Modify: `modules/nixos/cloud.nix:85,200-228`

- [ ] **Step 1: Replace hard-coded `overwrite.cli.url`**

In `modules/nixos/cloud.nix`, line 85, change:

```nix
"overwrite.cli.url" = "https://cloud.cirriform.au";
```

to:

```nix
"overwrite.cli.url" = "https://${cfg.fqdn}";
```

- [ ] **Step 2: Fix Collabora WOPI host and server_name**

In `modules/nixos/cloud.nix`, replace the Collabora config block (lines 200-219). Change:

```nix
    services.collabora-online = mkIf cfg.enableOffice {
      enable = true;
      settings = {
        ssl = {
          enable = false;
          termination = true;
        };

        net = {
          listen = "lookback";
          post_allow.host = ["127.0.0.1" "::1"];
        };

        sotrage.wopi = {
          "@allow" = true;
          host = ["cloud.cirriform.au"];
        };

        server_name = "collabora.cirriform.au";
      };
    };
```

to:

```nix
    services.collabora-online = mkIf cfg.enableOffice {
      enable = true;
      settings = {
        ssl = {
          enable = false;
          termination = true;
        };

        net = {
          listen = "lookback";
          post_allow.host = ["127.0.0.1" "::1"];
        };

        sotrage.wopi = {
          "@allow" = true;
          host = [cfg.fqdn];
        };

        server_name = "collabora.${cfg.fqdn}";
      };
    };
```

Note: `sotrage` is a typo in the original — preserve it to avoid breaking the config. The collabora `server_name` becomes `collabora.cloud.cirriform.au` which is slightly different from the original `collabora.cirriform.au`. If there's a DNS entry for `collabora.cirriform.au` specifically, you may want to keep it as-is or add a `collaboraFqdn` option. For now, derive it from `cfg.fqdn` for consistency.

- [ ] **Step 3: Fix Collabora nginx vhost**

In `modules/nixos/cloud.nix`, replace lines 223-228. Change:

```nix
    # TODO: get from catalog
    services.nginx.virtualHosts."collabora.cirriform.au" = mkIf cfg.enableOffice {
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.collabora-online.port}";
        proxyWebsockets = true;
      };
    };
```

to:

```nix
    services.nginx.virtualHosts."collabora.${cfg.fqdn}" = mkIf cfg.enableOffice {
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.collabora-online.port}";
        proxyWebsockets = true;
      };
    };
```

- [ ] **Step 4: Format**

Run: `nixfmt-rfc-style modules/nixos/cloud.nix`

- [ ] **Step 5: Verify the build**

Run: `nix flake check 2>&1 | tail -10`
Expected: no errors

- [ ] **Step 6: Commit**

```bash
git add modules/nixos/cloud.nix
git commit -m "Replace hard-coded domains in cloud.nix with cfg.fqdn"
```

---

### Task 2: Fix hard-coded paths in `modules/nixos/cloud.nix`

**Files:**
- Modify: `modules/nixos/cloud.nix:147-164`

- [ ] **Step 1: Fix `nextcloud-acl-setup` to derive paths from `cfg.dataPath`**

The current script hard-codes `/srv` and `/srv/data`. Replace the `nextcloud-acl-setup` systemd service (lines 147-164). Change:

```nix
    systemd.services.nextcloud-acl-setup = {
      description = "Setup base nextcloud acls for external storage";
      after = [ "systemd-tmpfiles-setup.service" ];
      wants = [ "systemd-tmpfiles-setup.service" ];
      before = [ "nextcloud-file-scan.service" ];
      wantedBy = [ "nextcloud-file-scan.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
      };

      script = ''
        echo "Setting nextcloud base ACLs"
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv
        ${pkgs.acl}/bin/setfacl -m u:nextcloud:x /srv/data
      '';
    };
```

to:

```nix
    systemd.services.nextcloud-acl-setup = let
      # Walk parent directories of dataPath to set execute ACLs
      # e.g. "/srv/nextcloud" -> ["/srv" "/srv/nextcloud"]
      # e.g. "/srv/data/nextcloud" -> ["/srv" "/srv/data" "/srv/data/nextcloud"]
      pathParts = lib.splitString "/" cfg.dataPath;
      nonEmptyParts = lib.filter (p: p != "") pathParts;
      parentPaths = lib.genList (
        i: "/" + lib.concatStringsSep "/" (lib.take (i + 1) nonEmptyParts)
      ) (lib.length nonEmptyParts);
      aclCommands = lib.concatMapStringsSep "\n" (
        path: "${pkgs.acl}/bin/setfacl -m u:nextcloud:x ${path}"
      ) parentPaths;
    in {
      description = "Setup base nextcloud acls for external storage";
      after = [ "systemd-tmpfiles-setup.service" ];
      wants = [ "systemd-tmpfiles-setup.service" ];
      before = [ "nextcloud-file-scan.service" ];
      wantedBy = [ "nextcloud-file-scan.service" ];

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = false;
      };

      script = ''
        echo "Setting nextcloud base ACLs"
        ${aclCommands}
      '';
    };
```

On terpsichore (dataPath resolves to `/srv/nextcloud`), this produces:
```
setfacl -m u:nextcloud:x /srv
setfacl -m u:nextcloud:x /srv/nextcloud
```

On calliope (dataPath was `/srv/data/nextcloud`), it would have produced:
```
setfacl -m u:nextcloud:x /srv
setfacl -m u:nextcloud:x /srv/data
setfacl -m u:nextcloud:x /srv/data/nextcloud
```

- [ ] **Step 2: Format**

Run: `nixfmt-rfc-style modules/nixos/cloud.nix`

- [ ] **Step 3: Verify the build**

Run: `nix flake check 2>&1 | tail -10`
Expected: no errors

- [ ] **Step 4: Commit**

```bash
git add modules/nixos/cloud.nix
git commit -m "Derive nextcloud ACL paths from cfg.dataPath"
```

---

### Task 3: Update `services.nix` host assignments

**Files:**
- Modify: `services.nix:11-13,27-29`

- [ ] **Step 1: Change cloud hosts**

In `services.nix`, line 12, change:

```nix
    hosts = [ "calliope" ];
```

to:

```nix
    hosts = [ "terpsichore" ];
```

- [ ] **Step 2: Change paperless-ngx hosts**

In `services.nix`, line 28, change:

```nix
    hosts = [ "calliope" ];
```

to:

```nix
    hosts = [ "terpsichore" ];
```

- [ ] **Step 3: Format**

Run: `nixfmt-rfc-style services.nix`

- [ ] **Step 4: Verify the build**

Run: `nix flake check 2>&1 | tail -10`
Expected: no errors

Also verify both host configs build:

Run: `nix build '.#nixosConfigurations.terpsichore.config.system.build.toplevel' --dry-run 2>&1 | tail -5`
Expected: no errors

Run: `nix build '.#nixosConfigurations.calliope.config.system.build.toplevel' --dry-run 2>&1 | tail -5`
Expected: no errors

- [ ] **Step 5: Commit**

```bash
git add services.nix
git commit -m "Reassign cloud and paperless-ngx to terpsichore"
```

---

### Task 4: Clean up calliope — remove bomftp, update PostgreSQL

**Files:**
- Delete: `hosts/nixos/calliope/bomftp.nix`
- Modify: `hosts/nixos/calliope/default.nix:9-35`

- [ ] **Step 1: Delete `bomftp.nix`**

Delete the file `hosts/nixos/calliope/bomftp.nix`.

- [ ] **Step 2: Remove bomftp import from calliope `default.nix`**

The file currently does not import `bomftp.nix` directly in the `imports` list (it's imported elsewhere or was already removed from imports). Search for any reference:

```bash
grep -rn bomftp hosts/nixos/calliope/
```

If found in `default.nix` imports, remove the line. If not found, the import may be in a different file — search and remove.

- [ ] **Step 3: Update PostgreSQL config on calliope**

In `hosts/nixos/calliope/default.nix`, lines 28-35, the PostgreSQL config currently pins pg14 with pgvector (needed for nextcloud). After migration, only keycloak needs pg. Keycloak uses `database.createLocally = true` which just needs `services.postgresql.enable = true` — it doesn't require a specific version or pgvector.

However, calliope's existing keycloak database was created under pg14. Upgrading the pg version requires a data migration on calliope. To avoid scope creep, keep pg14 for now but remove pgvector. Change:

```nix
  services.postgresql = {
    enable = true;
    # Removing this will triger the install of a newer version of postgresql without migrating the data
    package = pkgs.postgresql_14;
    extensions = with pkgs.postgresql_14.pkgs; [
      pgvector
    ];
  };
```

to:

```nix
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
  };
```

- [ ] **Step 4: Format**

Run: `nixfmt-rfc-style hosts/nixos/calliope/default.nix`

- [ ] **Step 5: Verify the build**

Run: `nix build '.#nixosConfigurations.calliope.config.system.build.toplevel' --dry-run 2>&1 | tail -5`
Expected: no errors

- [ ] **Step 6: Commit**

```bash
git rm hosts/nixos/calliope/bomftp.nix
git add hosts/nixos/calliope/default.nix
git commit -m "Remove bomftp and pgvector from calliope"
```

---

### Task 5: Clean up calliope backup.nix

**Files:**
- Modify: `hosts/nixos/calliope/backup.nix:14-16,39-41`

- [ ] **Step 1: Use dynamic PostgreSQL package**

In `hosts/nixos/calliope/backup.nix`, replace the hard-coded pg14 package (lines 14-16). Change:

```nix
  postgresWithExtensions = pkgs.postgresql_14.withPackages (p: [
    p.pgvector
  ]);
```

to:

```nix
  pg = config.services.postgresql.package;
```

This matches the pattern used in `hosts/nixos/terpsichore/backup.nix`.

- [ ] **Step 2: Update all references from `postgresWithExtensions` to `pg`**

Throughout the file, replace all occurrences of `${postgresWithExtensions}` with `${pg}`. There are 4 occurrences in the `preHook` script (lines 69, 70, 73, 76):

```nix
              ${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/pg_dumpall --globals-only > ${backupRWPath}/postgres_globals.sql
              ${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/pg_dumpall > ${backupRWPath}/postgres_all.sql
```

and in the loop:

```nix
              for db in $(${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/psql -t -c "select datname from pg_database where not datistemplate" | ${pkgs.gnugrep}/bin/grep '\S' | ${pkgs.gawk}/bin/awk '{$1=$1};1'); do
                echo "  Backing up database: $db"
                ${pkgs.sudo}/bin/sudo -u postgres ${pg}/bin/pg_dump --create --format=custom "$db" > "${backupRWPath}/$db.pgdump"
              done
```

- [ ] **Step 3: Format**

Run: `nixfmt-rfc-style hosts/nixos/calliope/backup.nix`

- [ ] **Step 4: Verify the build**

Run: `nix build '.#nixosConfigurations.calliope.config.system.build.toplevel' --dry-run 2>&1 | tail -5`
Expected: no errors

- [ ] **Step 5: Commit**

```bash
git add hosts/nixos/calliope/backup.nix
git commit -m "Use dynamic pg package in calliope backup"
```

---

### Task 6: Verify agenix secrets and full build

**Files:**
- Potentially modify: secrets submodule (agenix `secrets.nix`)

- [ ] **Step 1: Check terpsichore is a recipient for cloud/paperless secrets**

The secrets submodule must have terpsichore's host key as a recipient for these secret files:
- `secrets/admin-passwd.age` (shared by cloud + paperless)
- `secrets/nextcloud-secrets.json.age`
- `secrets/paperless.env.age`
- `secrets/paperless-api-token.age`
- `secrets/cf_origin_cert.pem.age` (gateway)
- `secrets/cf_origin_key.pem.age` (gateway)
- `secrets/acme-cf.env.age` (gateway)
- `secrets/fail2ban-cf.conf.age` (gateway)

Check the secrets submodule's `secrets.nix` for the recipient lists. If terpsichore is already a recipient for the gateway secrets (it runs other gateway-enabled services like immich), then only the cloud/paperless-specific ones need updating.

If re-encryption is needed:
```bash
cd secrets
agenix --rekey
git add -A
git commit -m "Re-encrypt secrets for terpsichore"
cd ..
git add secrets
git commit -m "Update secrets submodule"
```

- [ ] **Step 2: Full build verification for both hosts**

Run: `nix build '.#nixosConfigurations.terpsichore.config.system.build.toplevel' --dry-run 2>&1 | tail -10`
Expected: no errors

Run: `nix build '.#nixosConfigurations.calliope.config.system.build.toplevel' --dry-run 2>&1 | tail -10`
Expected: no errors

- [ ] **Step 3: Run flake check**

Run: `nix flake check 2>&1 | tail -10`
Expected: all checks pass

- [ ] **Step 4: Final commit (if any secret changes)**

```bash
git add -A
git commit -m "Verify builds for cloud/paperless migration"
```

---

### Task 7: Data migration (operational — run on hosts, not in Nix)

This task is not a code change. It documents the operational steps to run on the actual hosts after the Nix config changes are committed but before deploying.

- [ ] **Step 1: Pre-migration backup on calliope**

SSH to calliope and run:

```bash
sudo -u postgres pg_dump --create --format=custom nextcloud > /tmp/nextcloud.pgdump
sudo -u postgres pg_dump --create --format=custom paperless > /tmp/paperless.pgdump
sudo -u postgres pg_dumpall --globals-only > /tmp/postgres_globals.sql
```

- [ ] **Step 2: Stop services on calliope**

```bash
sudo systemctl stop nextcloud-setup nextcloud-cron paperless-web paperless-consumer paperless-task-queue paperless-scheduler
```

- [ ] **Step 3: Copy database dumps to terpsichore**

```bash
scp /tmp/nextcloud.pgdump /tmp/paperless.pgdump /tmp/postgres_globals.sql terpsichore:/tmp/
```

- [ ] **Step 4: Rsync file data to terpsichore**

```bash
rsync -avP --info=progress2 /srv/data/nextcloud/ terpsichore:/srv/nextcloud/
rsync -avP --info=progress2 /srv/data/paperless/ terpsichore:/srv/paperless/
```

- [ ] **Step 5: Restore databases on terpsichore**

SSH to terpsichore:

```bash
sudo -u postgres psql < /tmp/postgres_globals.sql
sudo -u postgres pg_restore --create -d postgres /tmp/nextcloud.pgdump
sudo -u postgres pg_restore --create -d postgres /tmp/paperless.pgdump
```

- [ ] **Step 6: Fix ownership on terpsichore**

```bash
sudo chown -R nextcloud:nextcloud /srv/nextcloud
sudo chown -R paperless:paperless /srv/paperless
```

- [ ] **Step 7: Deploy terpsichore**

From the nix config repo:

```bash
nix develop .#deploy -c deploy '.#terpsichore'
```

- [ ] **Step 8: Update DNS**

Update Cloudflare DNS for `cloud.cirriform.au` and `paperless.cirriform.au` to point to terpsichore's IP (`192.168.1.16`), or update the Cloudflare tunnel config if using tunnels.

- [ ] **Step 9: Verify services**

- Visit `https://cloud.cirriform.au` — verify login via OIDC works
- Visit `https://paperless.cirriform.au` — verify login via OIDC works
- Check nextcloud external storage scan: `sudo -u nextcloud nextcloud-occ files_external:list`
- Check paperless consumer is processing: `sudo systemctl status paperless-consumer`

- [ ] **Step 10: Deploy calliope (services removed)**

```bash
nix develop .#deploy -c deploy '.#calliope'
```

- [ ] **Step 11: Post-migration cleanup**

After confirming everything works for a few days, remove old data from calliope:

```bash
sudo rm -rf /srv/data/nextcloud /srv/data/paperless
```

Drop the old databases on calliope:

```bash
sudo -u postgres dropdb nextcloud
sudo -u postgres dropdb paperless
```
