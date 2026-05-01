# Migrate Cloud and Paperless to Terpsichore

## Summary

Move the `cloud` (Nextcloud) and `paperless-ngx` services from calliope (aarch64-linux, PostgreSQL 14) to terpsichore (x86_64-linux, PostgreSQL 16). Both services move together, preserving their same-host coupling. The `auth` (Keycloak) service remains on calliope.

## Scope

### In scope
- Update `services.nix` host assignments
- Fix hard-coded paths in `cloud.nix` to use `cfg.dataPath`/`cfg.fqdn` instead of literals
- Fix `nextcloud-acl-setup` to derive paths from config rather than hard-coding `/srv/data`
- Remove `bomftp.nix` from calliope
- Update calliope's `default.nix` to remove cloud/paperless-related config (PostgreSQL extensions, bomftp import)
- Update calliope's `backup.nix` to remove nextcloud/paperless-specific excludes and pg14 hard-coding
- Document the data migration procedure (pg_dump + rsync)

### Out of scope
- Decoupling cloud/paperless same-host dependency
- Moving the `auth` service
- Moving `wg-gateway` or `git-server`

## Current State

### services.nix assignments
- `cloud.hosts = ["calliope"]`, `dataPath = "/nextcloud"` -> resolves to `/srv/data/nextcloud`
- `paperless-ngx.hosts = ["calliope"]`, `dataPath = "/paperless"` -> resolves to `/srv/data/paperless`

### Host differences
| | calliope | terpsichore |
|---|---|---|
| Arch | aarch64-linux | x86_64-linux |
| dataPath (nodes.nix) | `/srv/data` | `/srv` |
| PostgreSQL | 14 + pgvector | 16 |
| Firewall 80/443 | closed | open |
| mail-relay | yes | yes |
| gateway (nginx) | no (provided by cloud/paperless modules) | yes (from existing services) |

### Hard-coded values to fix

**`modules/nixos/cloud.nix`:**
- Line 85: `"overwrite.cli.url" = "https://cloud.cirriform.au"` -> use `cfg.fqdn`
- Line 161: `setfacl -m u:nextcloud:x /srv` -> derive from `cfg.dataPath`
- Line 162: `setfacl -m u:nextcloud:x /srv/data` -> remove or derive from `cfg.dataPath`
- Line 215: `host = ["cloud.cirriform.au"]` (Collabora WOPI) -> use `cfg.fqdn`
- Line 218: `server_name = "collabora.cirriform.au"` -> derive from `cfg.fqdn`
- Line 223: `services.nginx.virtualHosts."collabora.cirriform.au"` -> derive from `cfg.fqdn`

**`hosts/nixos/calliope/bomftp.nix`:**
- Entire file to be removed

**`hosts/nixos/calliope/default.nix`:**
- PostgreSQL 14 + pgvector config -> only needed if other services on calliope still need pg (auth/keycloak uses pg, so keep pg but can drop pgvector and the version pin if keycloak works with newer pg)

**`hosts/nixos/calliope/backup.nix`:**
- Hard-coded `postgresql_14` with pgvector -> should use `config.services.postgresql.package` like terpsichore does

## Design

### Step 1: Fix hard-coded values in modules

**`modules/nixos/cloud.nix`:**
- Replace `"overwrite.cli.url" = "https://cloud.cirriform.au"` with `"overwrite.cli.url" = "https://${cfg.fqdn}"`
- Fix `nextcloud-acl-setup` to walk parent directories of `cfg.dataPath` dynamically instead of hard-coding `/srv` and `/srv/data`
- Replace Collabora hard-coded domains: derive `collaboraFqdn` as `"collabora.${services.services.domain}"` or add a `collaboraFqdn` option, and use `cfg.fqdn` for WOPI host
- Replace Collabora nginx vhost key with the derived domain

**`modules/nixos/paperless/default.nix`:**
- No path fixes needed — already uses `cfg.dataDir` throughout

### Step 2: Update `services.nix`

Change host assignments:
```nix
cloud = {
  hosts = [ "terpsichore" ];  # was "calliope"
  ...
};
paperless-ngx = {
  hosts = [ "terpsichore" ];  # was "calliope"
  ...
};
```

### Step 3: Update calliope host config

**`hosts/nixos/calliope/default.nix`:**
- Remove `./bomftp.nix` import (delete the file entirely)
- Evaluate PostgreSQL config: keycloak (auth) needs pg. Check if keycloak works with pg16 or if calliope still needs the pg14 pin. If keycloak is fine with a newer version, remove the version pin and pgvector extension.

**`hosts/nixos/calliope/backup.nix`:**
- Replace hard-coded `postgresql_14.withPackages` with `config.services.postgresql.package` (matching terpsichore's pattern)
- Remove `redis-nextcloud` and `redis-paperless` from borg exclude patterns (those services will no longer run on calliope)

### Step 4: Update terpsichore host config (if needed)

Terpsichore's `backup.nix` already uses `config.services.postgresql.package` dynamically. Its `localBackup.nix` excludes `redis-nextcloud` and `redis-paperless` already — these will now actually exist on terpsichore, so the excludes are correct.

No changes needed to terpsichore host config for the Nix side.

### Step 5: Agenix secrets

The secrets referenced by cloud and paperless (`admin-passwd.age`, `nextcloud-secrets.json.age`, `paperless.env.age`, `paperless-api-token.age`) are defined in `lib/serviceModules.nix` and automatically applied to whichever host runs the service. As long as these secrets are encrypted for terpsichore's host key (check the secrets submodule's `secrets.nix`), no module changes are needed.

Action: verify terpsichore's host key is in the agenix recipients for all cloud/paperless secrets. Re-encrypt if needed.

## Data Migration Procedure

This is an operational procedure, not a Nix config change. Execute after the config changes are built but before deploying terpsichore.

### Pre-migration
1. Take a final pg_dump on calliope for nextcloud and paperless databases
2. Stop nextcloud and paperless services on calliope

### Database migration
3. Copy pg_dump files from calliope to terpsichore via rsync/scp
4. On terpsichore, restore into PostgreSQL 16:
   - `pg_restore` for nextcloud db
   - `pg_restore` for paperless db
   - Restore roles from `postgres_globals.sql` (filter for nextcloud/paperless roles only)

### File data migration
5. rsync nextcloud data: `calliope:/srv/data/nextcloud/` -> `terpsichore:/srv/nextcloud/`
6. rsync paperless data: `calliope:/srv/data/paperless/` -> `terpsichore:/srv/paperless/`
7. Fix ownership on terpsichore: `chown -R nextcloud:nextcloud /srv/nextcloud`, `chown -R paperless:paperless /srv/paperless`

### Deploy and verify
8. Deploy terpsichore with the new config (`deploy .#terpsichore`)
9. Update DNS/Cloudflare to point `cloud.cirriform.au` and `paperless.cirriform.au` to terpsichore's IP
10. Verify services are working
11. Deploy calliope with the cleaned-up config (services removed)

### Post-migration cleanup
12. Remove nextcloud and paperless data from calliope after confirming migration success
13. Verify backups are running correctly on terpsichore

## Risks

- **Nextcloud external storage IDs**: The `nextcloud-file-scan` service scans external storage IDs 9 and 10 (paperless exports). These IDs are database-internal and should survive the pg_dump/restore, but verify after migration.
- **OIDC**: Auth stays on calliope at `auth.cirriform.au`. Cloud and paperless reference it by FQDN so this works cross-host. No change needed.
- **Collabora**: Runs locally on the cloud host. Will move with cloud to terpsichore. x86_64 binary availability is not a concern (collabora packages support x86_64).
- **Keycloak on calliope**: Needs PostgreSQL. Verify whether calliope can upgrade from pg14 after cloud/paperless move out, or if keycloak data also needs migration. This is independent of the cloud/paperless migration.
