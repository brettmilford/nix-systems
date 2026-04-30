{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.services.mail-relay;
in
{
  options.services.mail-relay = {
    enable = mkEnableOption "mail relay";

    relayhost = mkOption {
      type = types.str;
      example = "127.0.0.1:1025";
      description = "SMTP relay destination";
    };

    hostname = mkOption {
      type = types.str;
      example = "mail.example.com";
      description = "HELO/EHLO hostname";
    };

    domain = mkOption {
      type = types.str;
      example = "example.com";
      description = "Mail origin domain";
    };

    saslAuth = {
      enable = mkEnableOption "SASL authentication to relay";

      secretPath = mkOption {
        type = types.str;
        description = "Path to agenix secret containing SASL credentials";
      };
    };

    listenInterfaces = mkOption {
      type = types.listOf types.str;
      default = [ "localhost" ];
      description = "Interfaces postfix listens on";
    };

    mynetworks = mkOption {
      type = types.listOf types.str;
      default = [ "127.0.0.0/8" ];
      description = "Networks allowed to relay";
    };
  };

  config = mkIf cfg.enable {
    services.postfix = {
      enable = true;
      setSendmail = true;
      relayHost = cfg.relayhost;
      config =
        {
          myhostname = cfg.hostname;
          mydomain = cfg.domain;
          myorigin = "$mydomain";
          inet_protocols = "all";
          inet_interfaces = concatStringsSep ", " cfg.listenInterfaces;
          mynetworks = concatStringsSep ", " cfg.mynetworks;
          mydestination = "";
          bounce_sender = "admin@${cfg.domain}";
          smtp_tls_security_level = "may";
        }
        // optionalAttrs cfg.saslAuth.enable {
          smtp_sasl_auth_enable = "yes";
          smtp_sasl_security_options = "noanonymous";
          smtp_sasl_password_maps = "texthash:${cfg.saslAuth.secretPath}";
          smtp_sasl_mechanism_filter = "plain,login";
        };
    };
  };
}
