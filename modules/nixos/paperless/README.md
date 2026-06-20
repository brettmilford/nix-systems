# Paperless-NGX Module

## Overview

Paperless-NGX document management system with Proton Bridge IMAP integration.

Mail consumption uses two rules against the Proton account:

| Rule | Folder | Consumes |
|------|--------|----------|
| Paperless Consume Attachments | `Labels/Paperless Consume` | PDF/document attachments |
| Paperless Consume Email | `Labels/Paperless Email` | Entire email as PDF |

After processing, messages are moved to `Labels/Paperless`.

## Operations

### Trigger manual mail fetch

```bash
sudo -u paperless paperless-manage mail_fetcher
```

### Check mail processing logs

```bash
journalctl -u paperless-task-queue -n 100 | grep -E 'process_mail|ERROR|Consuming'
```

### Check processed mail history

```bash
sudo -u paperless psql -d paperless -c "
  SELECT pm.subject, pm.received, pm.status, LEFT(pm.error, 120) as error
  FROM paperless_mail_processedmail pm
  JOIN paperless_mail_mailrule mr ON pm.rule_id = mr.id
  JOIN paperless_mail_mailaccount ma ON mr.account_id = ma.id
  WHERE ma.name = 'Proton'
  ORDER BY pm.received DESC LIMIT 20;
"
```

## Known Issues

### Proton Bridge user service

The `protonmail-bridge` runs as a systemd **user** service under the `nix` user
(not a system service). Check its status with:

```bash
systemctl --user status protonmail-bridge
```

The bridge has no D-Bus secrets service or `pass` store available, so it runs
with an unencrypted vault. This is expected on a headless server.

Linger must be enabled for the service to survive without an active session:

```bash
loginctl enable-linger nix
```

---

## Troubleshooting: "No new documents were added"

### Symptom

`paperless-task-queue` logs show the mail task succeeding every 10 minutes with
`'No new documents were added.'`, but the Proton IMAP folders contain unread
messages.

### Root cause — IMAP UID collision after bridge reset

Paperless deduplicates mail using `(folder, uid)` stored in the
`paperless_mail_processedmail` table. Proton Bridge assigns IMAP UIDs locally
in its gluon database. **If the bridge is reinstalled, migrated, or its state
directory is cleared, UIDs are reassigned from 1.** New messages receive UIDs
that overlap with records already in `processedmail`, causing paperless to skip
them as "already seen."

Evidence: multiple subjects recorded for the same UID in `processedmail`
(inspect with the query above filtered to a single UID).

This has occurred at least three times, as shown by the distinct folder paths
in `processedmail`:

| Path | Era |
|------|-----|
| `Paperless` | Original setup |
| `Folders/Paperless` | After first bridge reset |
| `Labels/Paperless Consume` / `Labels/Paperless Email` | Current |

### Fix

Clear the stale `processedmail` records for the affected folders. Paperless
will reprocess all messages currently in those folders on the next run.
Duplicate documents are handled by paperless's content-hash deduplication.

```bash
sudo -u paperless psql -d paperless -c "
  DELETE FROM paperless_mail_processedmail
  WHERE id IN (
    SELECT pm.id
    FROM paperless_mail_processedmail pm
    JOIN paperless_mail_mailrule mr ON pm.rule_id = mr.id
    JOIN paperless_mail_mailaccount ma ON mr.account_id = ma.id
    WHERE ma.name = 'Proton'
      AND pm.folder IN ('Labels/Paperless Consume', 'Labels/Paperless Email')
  );
"
```

Then trigger an immediate fetch:

```bash
sudo -u paperless paperless-manage mail_fetcher
```

> **After any Proton Bridge migration or reinstall**, run the DELETE above
> before the next scheduled mail check to prevent the collision window.
