{
  description = "Nix systems config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix-darwin.url = "github:lnl7/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    lanzaboote.url = "github:nix-community/lanzaboote/v0.4.2";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
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
      top@{
        config,
        withSystem,
        moduleWithSystem,
        ...
      }:
      let
        users = import ./users.nix;
      in
      {
        imports = [ inputs.home-manager.flakeModules.home-manager ];

        systems = [
          "x86_64-linux"
          "aarch64-linux"
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
                  inputs = inputs;
                };
                modules = [
                  {
                    home.username = username;
                    home.homeDirectory = homeDirectory;
                    home.stateVersion = "25.05";
                    programs.home-manager.enable = true;
                    _module.args.userConfig = user // {
                      username = username;
                    };
                  }
                  self.homeModules.default
                ];
              };
          in
          {
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
              in
              pkgs.mkShell {
                packages = with pkgs; [
                  nixBin
                  nrs
                  inputs'.agenix.packages.default
                  inputs'.home-manager.packages.default
                  inputs'.deploy-rs.packages.default
                  nixfmt-tree
                  jq
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
            nodesBase = import ./nodes.nix { inherit self; };

            nodes = builtins.listToAttrs (
              map (hostname: {
                name = hostname;
                value = (nodesBase."${hostname}" // { hostname = hostname; });
              }) (builtins.attrNames nodesBase)
            );

            # Create serviceMap with nodes and lib
            serviceCatalog = import ./services.nix;

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

            # Standard arguments passed to all configurations
            commonSpecialArgs = {
              inherit
                self
                users
                nodes
                services
                ;
              inputs = inputs;
            };

            # Standard modules for all configurations
            commonModuleArgs = {
              _module.args = commonSpecialArgs;
            };

            # Filter nodes by system type
            nixosNodes = nixpkgs.lib.filterAttrs (name: host: nixpkgs.lib.hasInfix "linux" host.system) nodes;

            darwinNodes = nixpkgs.lib.filterAttrs (name: host: nixpkgs.lib.hasInfix "darwin" host.system) nodes;

            # Generate NixOS configuration
            mkNixosConfiguration =
              hostname: host:
              nixpkgs.lib.nixosSystem {
                system = host.system;
                specialArgs = commonSpecialArgs // {
                  inherit hostname;
                };
                modules = [
                  commonModuleArgs
                  self.nixosModules.default
                  ./hosts/nixos/${hostname}
                ]
                ++ (host.extraModules or [])
                ++ nixpkgs.lib.optional (services.lib.hasService hostname "desktop") self.nixosModules.users;
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
                  ./hosts/darwin/${hostname}
                ];
              };
          in
          {
            lib = {
              inherit users nodes services;
            };

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

            deploy.nodes = builtins.mapAttrs (hostname: host: {
              hostname = hostname;
              profiles.system = {
                user = "root";
                path = deploy-rs.lib.${host.system}.activate.nixos self.nixosConfigurations.${hostname};
                sshUser = "nix";
                remoteBuild = true;
                fastConnection = true;
              };
            }) nixosNodes;
          };
      }
    );
}
