{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.srht;

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
    in
    lib.nameValuePair "srht-container-${name}" {
      description = "SourceHut ${name}.sr.ht container";
      wantedBy = [ "multi-user.target" ];
      after = [
        "incus.service"
        "incus-preseed.service"
        "postgresql.service"
        "srht-db-setup.service"
        "redis-sourcehut.service"
        "age-secrets.target"
      ];
      requires = [ "incus.service" ];
      path = [
        config.virtualisation.incus.package
        pkgs.bash
        pkgs.coreutils
      ];

      script = ''
                if ! incus profile show default 2>/dev/null | grep -q 'root:'; then
                  incus profile device add default root disk path=/ pool=default size=10GiB
                fi

                if ! incus info ${containerName} &>/dev/null; then
                  incus launch images:alpine/edge ${containerName}
                  incus config set ${containerName} boot.autostart true
                  incus config device set ${containerName} eth0 ipv4.address=${svc.ip}
                  ${extraConfig}

                  incus exec ${containerName} -- sh -c '
                    setup-apkrepos -c -e
                    wget -qO /etc/apk/keys/alpine@sr.ht.rsa.pub \
                      https://mirror.sr.ht/alpine/sr.ht.rsa.pub
                    echo "https://mirror.sr.ht/alpine/edge/sr.ht" \
                      >> /etc/apk/repositories
                    apk update
                    apk add ${name}.sr.ht
                    rc-update add ${name}.sr.ht-api default
                    rc-update add ${name}.sr.ht default
                  '

                  incus exec ${containerName} -- sourcehut-migrate ${name}.sr.ht init
                fi

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
        redis-host = 10.0.100.1

        [webhooks]
        private-key = $SRHT_WEBHOOK_KEY

        [${name}.sr.ht]
        origin = https://${name}.${cfg.domain}
        connection-string = postgresql://srht:$SRHT_DB_PASSWORD@10.0.100.1/${name}_srht
        SRHTCFG

                incus file push /tmp/srht-${name}-config.ini ${containerName}/etc/sr.ht/config.ini

                rm -f /tmp/srht-${name}-config.ini

                incus start ${containerName} || true
                incus exec ${containerName} -- rc-service ${name}.sr.ht-api restart
                incus exec ${containerName} -- rc-service ${name}.sr.ht restart
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
