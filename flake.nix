{
  description = "Nix systems config";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-24-11.url = "github:NixOS/nixpkgs/nixos-24.11";
    nix-darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    lanzaboote.url = "github:nix-community/lanzaboote/v1.0.0";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      nixpkgs-24-11,
      nix-darwin,
      home-manager,
      agenix,
      nixos-generators,
      lanzaboote,
      flake-parts,
      deploy-rs,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      {
        config,
        withSystem,
        moduleWithSystem,
        ...
      }:
      let
        users = import ./users.nix;
      in
      {
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ];

        perSystem =
          {
            pkgs,
            system,
            inputs',
            ...
          }:
          let
            mkHome =
              username: user:
              let
                homeDirectory = if pkgs.stdenv.isDarwin then "/Users/${username}" else "/home/${username}";
              in
              home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                extraSpecialArgs = {
                  inherit self;
                };
                modules = [
                  {
                    home.username = username;
                    home.homeDirectory = homeDirectory;
                    home.stateVersion = "22.11";
                    _module.args.userConfig = user // {
                      username = username;
                    };
                  }
                  self.homeModules.default
                ];
              };
          in
          {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = builtins.attrValues self.overlays;
            };

            legacyPackages.homeConfigurations = builtins.mapAttrs (username: user: mkHome username user) users;

            devShells.default =
              let
                nixBin = pkgs.writeShellScriptBin "nix" ''
                  ${pkgs.nixVersions.stable}/bin/nix --option experimental-features "nix-command flakes" "$@"
                '';
                nrs = pkgs.writeShellScriptBin "nrs" (
                  if pkgs.stdenv.isDarwin then
                    ''
                      sudo darwin-rebuild switch --flake "''${FLAKE}" "$@"
                    ''
                  else
                    ''
                      sudo nixos-rebuild switch --flake "''${FLAKE}" "$@"
                    ''
                );
                deploy-diff = pkgs.writeShellScriptBin "deploy-diff" ''
                  #!${pkgs.bash}/bin/bash
                  host=$2
                  if [ -z "$host" ]; then
                    host=$1
                  fi
                  set -eou pipefail

                  trap 'rm wait.fifo' EXIT
                  mkfifo wait.fifo

                  deploy --debug-logs --dry-activate ".#$1" 2>&1 \
                    | tee >(grep -v DEBUG) >(grep 'activate-rs --debug-logs activate' | \
                        sed -e 's/^.*activate-rs --debug-logs activate \(.*\) --profile-user.*$/\1/' | \
                        xargs -I% bash -xc "ssh $host 'nix store diff-closures /run/current-system %'" ; echo >wait.fifo) \
                    >/dev/null

                  read <wait.fifo
                '';
              in
              pkgs.mkShell {
                packages = with pkgs; [
                  nixBin
                  nrs
                  deploy-diff
                  inputs'.agenix.packages.default
                  inputs'.home-manager.packages.default
                  inputs'.deploy-rs.packages.default
                  nixfmt-tree
                  jq
                  git
                ];
                shellHook = ''
                  export PS1='\[\033[1;32m\](nix-systems)[\u@\h:\w]\$\[\033[0m\] '
                  export FLAKE="$(pwd)"
                  alias hms='home-manager switch --flake "''${FLAKE}?submodules=1#''${USER}"'
                  alias nup='nix flake update --flake "''${FLAKE}" && nrs'
                  alias nvm='nix run ".#nixosConfigurations.$(hostname -s).config.system.build.vmWithBootLoader"'
                '';
              };
            formatter = pkgs.nixfmt-rfc-style;
            checks = {
              libTests =
                import ./lib/tests/release.nix { inherit pkgs; }
                // deploy-rs.lib.${system}.deployChecks self.deploy;
            };
          };

        flake =
          let
            nodes = import ./nodes.nix { inherit self; };

            # Create services with nodes and lib
            serviceCatalog = import ./services.nix;

            # TODO: refac to catalog
            services = {
              inherit nodes;
              services = serviceCatalog;

              # Wrap functions under lib
              lib = import ./lib/serviceMap.nix {
                lib = nixpkgs.lib;
                inherit nodes;
                services = serviceCatalog;
              };
            };

            # Import the service modules factory
            serviceModulesLib = import ./lib/serviceModules.nix {
              inherit (nixpkgs) lib;
              inherit nodes services;
            };

            # Standard arguments passed to all configurations
            commonSpecialArgs = {
              inherit
                inputs
                self
                users
                nodes
                services
                ;
            };

            # Standard modules for all configurations
            commonModuleArgs = {
              _module.args = commonSpecialArgs;
            };

            # Filter nodes by system type
            nixosNodes = nixpkgs.lib.filterAttrs (
              nodeName: node: nixpkgs.lib.hasInfix "linux" node.system
            ) nodes;

            darwinNodes = nixpkgs.lib.filterAttrs (
              nodeName: node: nixpkgs.lib.hasInfix "darwin" node.system
            ) nodes;

            # Generate NixOS configuration
            mkNixosConfiguration =
              hostname: host:
              let
                # Get modules and options for this host based on services
                hostModules = serviceModulesLib.createModulesForHost hostname;
              in
              nixpkgs.lib.nixosSystem {
                system = host.system;
                specialArgs = commonSpecialArgs // {
                  inherit hostname;
                };
                modules = [
                  commonModuleArgs
                  self.nixosModules.default
                  {
                    nixpkgs.overlays = [ self.overlays.default ];
                  }
                  ./hosts/nixos/${hostname}
                ]
                ++ hostModules.modules
                ++ [
                  hostModules.optionsModule
                ]
                ++ (host.extraModules or [ ]);
              };
            # Generate Darwin configuration
            mkDarwinConfiguration =
              hostname: host:
              nix-darwin.lib.darwinSystem {
                system = host.system;
                specialArgs = commonSpecialArgs // {
                  inherit hostname;
                  pkgs-x86_64 = import nixpkgs { system = "x86_64-darwin"; };
                };
                modules = [
                  commonModuleArgs
                  self.darwinModules.default
                  {
                    nixpkgs.overlays = [ self.overlays.default ];
                  }
                  ./hosts/darwin/${hostname}
                ];
              };
          in
          {
            overlays.default = import ./overlays { inherit inputs; };

            homeModules.default = {
              imports = [
                ./modules/home
              ];
            };

            darwinModules.default = {
              imports = [
                agenix.darwinModules.default
                ./modules/darwin
              ];
            };

            nixosModules = {
              default = {
                imports = [
                  agenix.nixosModules.default
                  ./modules/nixos
                ];
              };

              users = {
                imports = [
                  ./modules/nixos/users.nix
                ];
              };

              secureBoot = {
                imports = [
                  lanzaboote.nixosModules.lanzaboote
                ];
              };
            };

            nixosConfigurations = builtins.mapAttrs mkNixosConfiguration nixosNodes;

            darwinConfigurations = builtins.mapAttrs mkDarwinConfiguration darwinNodes;

            deploy.nodes = builtins.mapAttrs (
              nodeName: node:
              let
                hasUsers = builtins.elem self.nixosModules.users (node.extraModules or [ ]);
                userProfiles =
                  if hasUsers then
                    builtins.mapAttrs (username: userConfig: {
                      user = username;
                      path =
                        deploy-rs.lib.${node.system}.activate.home-manager
                          self.legacyPackages.${node.system}.homeConfigurations.${username};
                      sshUser = username;
                      remoteBuild = true;
                      fastConnection = true;
                    }) users
                  else
                    { };
                systemType = if nixpkgs.lib.hasInfix "linux" node.system then "nixos" else "darwin";
              in
              {
                hostname = nodeName;
                profiles = {
                  system = {
                    user = "root";
                    path =
                      deploy-rs.lib.${node.system}.activate.${systemType}
                        self."${systemType}Configurations".${nodeName};
                    sshUser = "nix";
                    remoteBuild = true;
                    fastConnection = true;
                    activationTimeout = 600;
                    confirmTimeout = 60;
                  };
                }
                // userProfiles;
              }
            ) nodes;
          };
      }
    );
}
