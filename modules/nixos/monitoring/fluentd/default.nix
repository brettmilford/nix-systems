{ config, lib, pkgs, ... }:
let
  fluentdWithPlugins = pkgs.bundlerEnv {
    inherit (pkgs) ruby;
    pname = "fluentd";
    gemdir = ./.;
    buildInputs = [ pkgs.systemd.dev  pkgs.makeWrapper ];
    nativeBuildInputs = [ pkgs.pkg-config pkgs.makeWrapper];
    postBuild = ''
      if [ -z "''${wrapProgram-}" ]; then
        source ${pkgs.makeWrapper}/nix-support/setup-hook
      fi
      wrapProgram $out/bin/fluentd \
        --set LD_LIBRARY_PATH "${pkgs.systemd}/lib" \
        --prefix PKG_CONFIG_PATH : "${pkgs.systemd.dev}/lib/pkgconfig"
    '';
    passthru.updateScript = pkgs.bundlerUpdateScript "fluentd";
    passthru.tests.fluentd = pkgs.nixosTests.fluentd;
  };

  netflowDefinitions = pkgs.writeText "netflow-definitions.yaml" ''
    # Custom netflow definition to skip bad options
    ---
    option:
      256:
      - :skip
      259:
      - :skip
  '';

in
{
  networking.firewall = {
    allowedUDPPorts = [ 1514 2055 ];
    allowedTCPPorts = [ 1514 ];
  };

  services.fluentd = {
    package = fluentdWithPlugins;

    config = ''
      # Ignore fluentd own events
      <label @FLUENT_LOG>
        <match fluent.*>
          @type null
        </match>
      </label>

      # Netflow input
      <source>
        @type netflow
        tag netflow.event
        bind 0.0.0.0
        port 2055
        # cache_ttl 4000
        # versions [5, 9]
        definitions ${netflowDefinitions}
        switched_times_from_uptime yes
      </source>
      <match netflow.**>
        @type loki
        line_format json
        url "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}"
        extra_labels {
          "job": "fluentd",
          "source": "netflow",
          "app": "netflow",
          "level": "info"
        }
        <label>
          tag
          host
          protocol_name
          traffic_class
          port_category
          src_network
          dst_network
        </label>
        # remove_keys ipv4_src_addr,ipv4_dst_addr,l4_src_port,l4_dst_port
        <buffer tag,host,protocol_name,traffic_class,port_category>
          @type memory
          flush_interval 5s
          flush_at_shutdown true
          chunk_limit_size 2m
          retry_wait 2
          compress gzip
        </buffer>
      </match>

      # Syslog input
      <source>
        @type syslog
        port 1514
        <transport udp>
        </transport>
        bind 0.0.0.0
        tag syslog
        <parse>
          @type syslog
          message_format auto
          parser_type string
        </parse>
        severity_key severity
        facility_key facility
        emit_unmatched_lines true
      </source>
      <source>
        @type syslog
        port 1514
        <transport tcp>
        </transport>
        send_keepalive_packet true
        bind 0.0.0.0
        tag syslog
        <parse>
          @type syslog
          message_format auto
          parser_type string
        </parse>
        severity_key severity
        facility_key facility
        emit_unmatched_lines true
      </source>
      # Syslog output
      <filter **>
        @type record_transformer
        <record>
          tag ''${tag}
        </record>
      </filter>
      <match syslog.**>
        @type loki
        url "http://127.0.0.1:${toString config.services.loki.configuration.server.http_listen_port}"
        extra_labels {"job": "fluentd", "source": "syslog"}
        <label>
          tag
          host
          app $.ident
          level $.severity
          facility
        </label>
        <buffer>
          @type memory
          flush_interval 10s
          flush_at_shutdown true
          chunk_limit_size 1m
          retry_wait 2
        </buffer>
      </match>

      # Journald inputs
      <source>
        @type systemd
        tag system.journald
        read_from_head false
        <storage>
          @type local
          persistent true
          path /var/log/td-agent/tmp/system.journald.pos
        </storage>
        <entry>
          fields_strip_underscores true
          fields_lowercase true
        </entry>
      </source>

      # Send to Loki with organized labels
      <match system.journald>
        @type loki
        line_format json
        url "http://127.0.0.1:3100"
        extra_labels {
          "job": "fluentd",
          "source": "systemd"
        }
        <label>
          transport
          systemd_unit
          kernel_subsystem
          container_name
          priority
          hostname
        </label>
        # Remove service_name from buffer since it's not being used
        <buffer tag,transport_type,service,kernel_subsystem,container_name,log_source>
          @type memory
          flush_interval 5s
          flush_at_shutdown true
          chunk_limit_size 2m
          retry_wait 2
          compress gzip
        </buffer>
      </match>

# Nginx access logs
<source>
  @type tail
  path /var/log/nginx/access.log
  pos_file /var/log/td-agent/tmp/nginx-access.log.pos
  tag nginx.access
  <parse>
    @type nginx
  </parse>
</source>

# Nginx error logs
<source>
  @type tail
  path /var/log/nginx/error.log
  pos_file /var/log/td-agent/tmp/nginx-error.log.pos
  tag nginx.error
  <parse>
    @type multiline
    format_firstline /^\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2}/
    format1 /^(?<time>\d{4}\/\d{2}\/\d{2} \d{2}:\d{2}:\d{2}) \[(?<level>\w+)\] (?<pid>\d+)#(?<tid>\d+): (\*(?<cid>\d+) )?(?<message>.*)/
  </parse>
</source>

# Process nginx logs without Ruby expressions
<filter nginx.**>
  @type record_transformer
  <record>
    service nginx
    hostname eurydice
  </record>
</filter>

# Add log_type based on tag
<filter nginx.access>
  @type record_transformer
  <record>
    log_type access
    response_category unknown
    error_level ""
  </record>
</filter>

<filter nginx.error>
  @type record_transformer
  <record>
    log_type error
    response_category ""
    error_level info
  </record>
</filter>

# Send nginx logs to Loki
<match nginx.**>
  @type loki
  line_format json
  url "http://127.0.0.1:3100"
  extra_labels {
    "job": "fluentd",
    "source": "nginx"
  }
  <label>
    service
    log_type
    hostname
    response_category
    error_level
  </label>
  <buffer service,log_type,hostname,response_category,error_level>
    @type memory
    flush_interval 5s
    flush_at_shutdown true
    chunk_limit_size 2m
    retry_wait 2
    compress gzip
  </buffer>
</match>
      '';
  };
}
