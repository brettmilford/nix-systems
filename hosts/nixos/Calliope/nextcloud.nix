{
  pkgs,
  config,
  lib,
  ...
}: {
  age.secrets.nextcloud = {
    file = ../../../secrets/nextcloud.age;
    owner = "nextcloud";
    group = "nextcloud";
  };

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud27;
    hostName = "nextcloud.cirriform.au";
    config = {
      dbtype = "pgsql";
      adminpassFile = config.age.secrets.nextcloud.path;
      dbhost = "/run/postgresql";
      defaultPhoneRegion = "AU";
      extraTrustedDomains = ["localhost" "calliope" "calliope.cirriform" "calliope.cirriform.au"];
    };
    appstoreEnable = true;
    extraApps = {
      inherit
        (pkgs.nextcloud27Packages.apps)
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

  age.secrets."cf_origin_cert" = {
    file = ../../../secrets/cf_origin_cert.pem.age;
    mode = "770";
    owner = "nginx";
    group = "nginx";
  };

  age.secrets."cf_origin_key" = {
    file = ../../../secrets/cf_origin_key.pem.age;
    mode = "770";
    owner = "nginx";
    group = "nginx";
  };

  services.nginx.virtualHosts.${config.services.nextcloud.hostName} = {
    sslCertificate = config.age.secrets."cf_origin_cert".path;
    sslCertificateKey = config.age.secrets."cf_origin_key".path;
  };

  services.nginx.commonHttpConfig = let
    realIpsFromList = lib.strings.concatMapStringsSep "\n" (x: "set_real_ip_from  ${x};");
    fileToList = x: lib.strings.splitString "\n" (builtins.readFile x);
    cfipv4 = fileToList (pkgs.fetchurl {
      url = "https://www.cloudflare.com/ips-v4";
      sha256 = "0ywy9sg7spafi3gm9q5wb59lbiq0swvf0q3iazl0maq1pj1nsb7h";
    });
    cfipv6 = fileToList (pkgs.fetchurl {
      url = "https://www.cloudflare.com/ips-v6";
      sha256 = "1ad09hijignj6zlqvdjxv7rjj8567z357zfavv201b9vx3ikk7cy";
    });
  in ''
    ${realIpsFromList cfipv4}
    ${realIpsFromList cfipv6}
    real_ip_header CF-Connecting-IP;
  '';

  age.secrets."cfApiKey".file = ../../../secrets/cfApiKey.age;
  services.fail2ban = let
    cfEmail = "brettmilford@gmail.com";
    cfApiKey = config.age.secrets."cfApiKey".path;
  in {
    enable = true;
    extraPackages = [pkgs.curl pkgs.ipset];
    banaction = "iptables-ipset-proto6-allports";
    ignoreIP = [
      "172.22.70.58/16"
    ];

    jails.nginx-noagent = ''
      enabled  = true
      port     = http,https
      filter   = nginx-noagent
      backend  = auto
      maxretry = 1
      logpath  = %(nginx_access_log)s
      action   = cloudflare[cfuser="${cfEmail}", cftoken="${cfApiKey}"]
                 iptables-multiport[port="http,https"]
    '';
  };

  environment.etc."fail2ban/filter.d/nginx-noagent.conf".text = ''
    [Definition]

    failregex = ^<HOST> -.*"-" "-"$

    ignoreregex =
  '';

  programs.msmtp = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [
    imagemagick
    ffmpeg_6-headless
    libde265
    libheif
  ];

  networking.firewall.allowedTCPPorts = [80 443];
}
