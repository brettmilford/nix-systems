{ self, config, lib, pkgs, ... }:

{
  age.secrets.postfix-sasl-passwd = {
    file = "${self}/secrets/postfix-sasl-passwd.age";
  };

  # systemctl --user start protonmail-bridge
  # loginctl enable-linger nix
  services.protonmail-bridge = {
    enable = true;
    path = [ pkgs.pass ];
    logLevel = "info";
  };

  services.postfix = {
    enable = true;
    setSendmail = true;

    settings.main.relayhost = [ "127.0.0.1:1025" ];

    config = {
      myhostname = "mail.cirriform.au";
      mydomain = "cirriform.au";
      myorigin = "$mydomain";
      # Network settings
      inet_protocols = "all";
      inet_interfaces = "localhost";
      mydestination = "";

      smtp_sasl_auth_enable = "yes";
      smtp_sasl_security_options = "noanonymous";
      smtp_sasl_password_maps = "texthash:${config.age.secrets.postfix-sasl-passwd.path}";
      smtp_use_tls = "no";
      smtp_sasl_mechanism_filter = "plain,login";
      bounce_sender = "admin@cirriform.au";

      # Bridge usually uses STARTTLS
      smtp_tls_security_level = "may";
    };
  };
}
