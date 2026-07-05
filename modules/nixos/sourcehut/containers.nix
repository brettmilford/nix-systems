{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.srht;

  alpineRelease = "3.22";

  mkContainerService =
    name: svc:
    let
      containerName = "srht-${name}";
      extraConfig =
        if name == "builds" then
          ''
            incus config set ${containerName} security.nesting=true
            incus config device add ${containerName} kvm unix-char path=/dev/kvm 2>/dev/null || true
          ''
        else
          "";

      serviceExtraIni =
        if name == "meta" then
          ''

            [meta.sr.ht::settings]
            onboarding-redirect = https://${name}.${cfg.domain}
            registration = no

            [meta.sr.ht::billing]
            enabled = no
          ''
        else if name == "git" then
          ''
            outgoing-domain = https://git.${cfg.domain}
            post-update-script = /usr/bin/git.sr.ht-update-hook
            repos = /var/lib/git
          ''
        else if name == "todo" then
          ''

            [todo.sr.ht::mail]
            posting-domain = todo.${cfg.domain}
          ''
        else
          "";

      # Every service needs to resolve the origins of all other services (for
      # cross-service links and auth), but only connects to its own database.
      serviceSections = lib.concatStrings (
        lib.mapAttrsToList (
          sname: _sdef:
          ''
            [${sname}.sr.ht]
            origin = https://${sname}.${cfg.domain}
          ''
          + lib.optionalString (
            sname == name
          ) "connection-string = postgresql://srht:$SRHT_DB_PASSWORD@10.0.100.1/${name}_srht?sslmode=disable\n"
          + lib.optionalString (sname == name) serviceExtraIni
        ) cfg.serviceDefs
      );
    in
    lib.nameValuePair "srht-container-${name}" {
      description = "SourceHut ${name}.sr.ht container";
      wantedBy = [ "multi-user.target" ];
      restartTriggers = [ config.age.secrets."srht-secrets-env".file ];
      after = [
        "incus.service"
        "incus-preseed.service"
        "postgresql.service"
        "srht-db-setup.service"
        "redis-sourcehut.service"
        "age-secrets.target"
      ];
      requires = [
        "incus.service"
        "srht-db-setup.service"
      ];
      path = [
        config.virtualisation.incus.package
        pkgs.bash
        pkgs.coreutils
      ];

      script = ''
                set -e

                if ! incus profile show default 2>/dev/null | grep -q 'root:'; then
                  incus profile device add default root disk path=/ pool=default size=10GiB
                fi

                if incus info ${containerName} &>/dev/null; then
                  release=$(incus config get ${containerName} image.release 2>/dev/null || true)
                  address=$(incus config device get ${containerName} eth0 ipv4.address 2>/dev/null || true)
                  if [ "$release" != "${alpineRelease}" ] || [ "$address" != "${svc.ip}" ]; then
                    incus delete --force ${containerName}
                  fi
                fi

                if ! incus info ${containerName} &>/dev/null; then
                  incus init images:alpine/${alpineRelease} ${containerName}
                  incus config set ${containerName} boot.autostart=true
                  incus config device override ${containerName} eth0 ipv4.address=${svc.ip}
                  ${extraConfig}
                fi

                incus start ${containerName} 2>/dev/null || true

                incus exec ${containerName} -- sh -c '
                  set -e
                  for i in $(seq 1 30); do
                    nslookup dl-cdn.alpinelinux.org >/dev/null 2>&1 && break
                    sleep 2
                  done
                  setup-apkrepos -c -1
                  wget -qO /etc/apk/keys/alpine@sr.ht.rsa.pub \
                    https://mirror.sr.ht/alpine/alpine@sr.ht.rsa.pub
                  grep -q "mirror.sr.ht/alpine/v${alpineRelease}/sr.ht" /etc/apk/repositories \
                    || sed -i "1i https://mirror.sr.ht/alpine/v${alpineRelease}/sr.ht" /etc/apk/repositories
                  apk update
                  apk add ${name}.sr.ht
                '

                set -f
                source ${cfg.secretPaths."srht-secrets-env"}
                set +f

                cat > /tmp/srht-${name}-config.ini <<SRHTCFG
        [sr.ht]
        site-name = sourcehut
        site-info = https://${cfg.domain}
        site-blurb = the hacker's forge
        environment = production
        owner-name = Brett
        owner-email = brett@cirriform.au
        source-url = https://sr.ht/~sircmpwn/sourcehut
        service-key = $SRHT_SERVICE_KEY
        network-key = $SRHT_NETWORK_KEY
        redis-host = redis://10.0.100.1:6379

        [mail]
        smtp-host = 10.0.100.1
        smtp-port = 25
        smtp-from = admin@${cfg.domain}

        [webhooks]
        private-key = $SRHT_WEBHOOK_KEY

        ${serviceSections}
        SRHTCFG

                incus file push --create-dirs /tmp/srht-${name}-config.ini ${containerName}/etc/sr.ht/config.ini

                rm -f /tmp/srht-${name}-config.ini

                incus exec ${containerName} -- sh -c '
                  f=/etc/conf.d/${name}.sr.ht
                  if [ -f "$f" ]; then
                    if grep -q "^${lib.toUpper name}_BIND=" "$f"; then
                      sed -i "s|^${lib.toUpper name}_BIND=.*|${lib.toUpper name}_BIND=0.0.0.0:${toString svc.webPort}|" "$f"
                    else
                      printf "%s\n" "${lib.toUpper name}_BIND=0.0.0.0:${toString svc.webPort}" >> "$f"
                    fi
                  fi
                '

                incus exec ${containerName} -- sourcehut-migrate ${name}.sr.ht init || true

                for unit in ${name}.sr.ht-api ${name}.sr.ht; do
                  if incus exec ${containerName} -- rc-service --exists "$unit"; then
                    incus exec ${containerName} -- rc-update add "$unit" default
                    incus exec ${containerName} -- rc-service "$unit" restart
                  fi
                done
      '';

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
    };

  containerServices = lib.mapAttrs' mkContainerService cfg.serviceDefs;
in
{
  config = mkIf cfg.enable {
    systemd.services = containerServices;
  };
}
