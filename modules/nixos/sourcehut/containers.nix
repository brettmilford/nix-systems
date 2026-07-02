{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.sourcehut;

  mkContainerService =
    name: svc:
    let
      containerName = "srht-${name}";
      secretName = "srht-${name}-config";
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
        "redis-sourcehut.service"
      ];
      requires = [ "incus.service" ];
      path = [
        config.virtualisation.incus.package
        pkgs.bash
      ];

      script = ''
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

        incus file push \
          ${cfg.secretPaths.${secretName}} \
          ${containerName}/etc/sr.ht/config.ini

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
