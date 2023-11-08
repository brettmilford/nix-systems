{ pkgs, config, ... }:
{
  age.secrets.nextcloud.file = ../../../secrets/nextcloud.age;

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud27;
    hostName = "nextcloud.cirriform.au";
    config = {
      dbtype = "pgsql";
      adminpassFile = config.age.secrets.nextcloud.path;
      dbhost = "/run/postgresql";
      defaultPhoneRegion = "AU";
      extraTrustedDomains = [ "localhost" "calliope" "calliope.cirriform" "calliope.cirriform.au"];
    };
    appstoreEnable = true;
    extraApps = { inherit (pkgs.nextcloud27Packages.apps)
      calendar
      contacts
      ;
    };
    extraAppsEnable = true;
    enableImagemagick = true;
    configureRedis = true;
    https = true;
    datadir = "/srv/data/nextcloud";
    extraOptions = {
      mail_smtpmode = "sendmail";
      mail_sendmailmode = "pipe";
      mail_from_address = "admin";
      mail_domain = "cirriform.au";
      enable_previews = true;
      enabledPreviewProviders = [
          "OC\\Preview\\PNG"
          "OC\\Preview\\JPEG"
          "OC\\Preview\\GIF"
          "OC\\Preview\\BMP"
          "OC\\Preview\\XBitmap"
          "OC\\Preview\\MP3"
          "OC\\Preview\\TXT"
          "OC\\Preview\\MarkDown"
          "OC\\Preview\\OpenDocument"
          "OC\\Preview\\Krita"
          "OC\\Preview\\HEIC"
          "OC\\Preview\\PDF"
          "OC\\Preview\\Movie"
          "OC\\Preview\\MKV"
          "OC\\Preview\\MP4"
          "OC\\Preview\\AVI"
          "OC\\Preview\\MSOfficeDoc"
      ];
    };
    phpOptions = {
      catch_workers_output = "yes";
      display_errors = "stderr";
      error_reporting = "E_ALL & ~E_DEPRECATED & ~E_STRICT";
      expose_php = "Off";
      "opcache.enable_cli" = "1";
      "opcache.fast_shutdown" = "1";
      "opcache.interned_strings_buffer" = "16";
      "opcache.max_accelerated_files" = "10000";
      "opcache.memory_consumption" = "128";
      "opcache.revalidate_freq" = "1";
      "openssl.cafile" = "/etc/ssl/certs/ca-certificates.crt";
      short_open_tag = "Off";
    };
  };

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "brettmilford@gmail.com";

  services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
    forceSSL = true;
    enableACME = true;
  };

  programs.msmtp = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [
    imagemagick
    ffmpeg_6-headless
    libde265
    libheif
  ];

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
