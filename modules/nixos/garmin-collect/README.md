# Backfilling data

```sh
sudo systemd-run \
  --unit=garmin-backfill \
  --property=User=garmin \
  --property=Group=garmin \
  --property=EnvironmentFile=/run/agenix/garmin-collect.env \
  --property=Environment=DATABASE_URL=postgresql:///garmin?host=/run/postgresql \
  --property=Environment=GARMIN_TOKEN_DIR=/var/lib/garmin-collect \
  $(systemctl cat garmin-collect | awk -F= '/ExecStart=/{print $2}')
  --backfill 90
```
