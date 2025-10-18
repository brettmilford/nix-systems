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
                  inputs'.agenix.packages.default
                  inputs'.home-manager.packages.default
                  nixfmt-tree
                ];
                shellHook = ''
                  export FLAKE="$(pwd)"
                  export PATH="${nrs}/bin:${nixBin}/bin:$PATH"
                  export PS1='\[\033[1;32m\][nix-systems:\w]\$\[\033[0m\] '
                  alias hms='home-manager switch --flake "''${FLAKE}?submodules=1#''${USER}"'
                  alias nup='nix flake update --flake "''${FLAKE}" && nrs'
                '';
              };
            formatter = pkgs.nixfmt-rfc-style;
          };

        flake = {
          homeModules.default = {
            imports = [
              ./modules/home
            ];
          };

          darwinModules.default = {
            imports = [
              ./modules/darwin
            ];
          };

          darwinConfigurations = {
            "thamrys" = nix-darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              specialArgs = {
                inherit users;
                pkgs-x86_64 = import nixpkgs { system = "x86_64-darwin"; };
              };
              modules = [
                self.darwinModules.default
                agenix.darwinModules.default
                ./hosts/darwin/thamrys
              ];
            };
          };

          nixosConfigurations =
            let
              nixosCommonModules = [
                {
                  system.stateVersion = "25.05";
                  nix = {
                    extraOptions = ''
                      extra-platforms = aarch64-linux x86_64-linux
                      experimental-features = nix-command flakes
                    '';
                    settings.auto-optimise-store = true;
                    gc = {
                      automatic = true;
                      dates = "weekly";
                      options = "--delete-older-than 30d";
                    };
                  };
                }
                agenix.nixosModules.default
              ];
              nixosUserModules =
                {
                  user,
                  desc,
                }:
                [
                  {
                    users.users.${user} = {
                      home = "/home/${user}";
                      isNormalUser = true;
                      group = "${user}";
                      description = "${desc}";
                      extraGroups = [
                        "wheel"
                        "networkmanager"
                      ];
                    };
                    users.groups.${user} = { };
                  }
                ];
            in
            {

              "orpheus" = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules =
                  nixosCommonModules
                  ++ nixosUserModules {
                    user = "brett";
                    desc = "Brett";
                  }
                  ++ [
                    ./hosts/nixos/orpheus
                  ];
              };

              "orpheus-vm" = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules =
                  nixosCommonModules {
                    user = "brett";
                    desc = "Brett";
                  }
                  ++ [
                    ./hosts/nixos/orpheus
                    ./hosts/nixos/build-vm.nix
                  ];
              };

              "eurydice" = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules = nixosCommonModules ++ [
                  ./hosts/nixos/eurydice
                ];
                specialArgs = {
                  inherit self;
                  inputs = inputs;
                };
              };

              "calliope" = nixpkgs.lib.nixosSystem {
                system = "aarch64-linux";
                modules = nixosCommonModules ++ [
                  ./hosts/nixos/calliope
                ];
                specialArgs = {
                  inherit self;
                  inputs = inputs;
                };
              };

              "terpsichore" = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules = nixosCommonModules ++ [
                  ./hosts/nixos/terpsichore
                  lanzaboote.nixosModules.lanzaboote
                ];
                specialArgs = {
                  inherit self;
                  inputs = inputs;
                };
              };

              "dev" = nixpkgs.lib.nixosSystem {
                system = "aarch64-linux";
                modules =
                  nixosCommonModules {
                    user = "brett";
                    desc = "Brett";
                  }
                  ++ [
                    {
                      imports = [ nixos-generators.nixosModules.all-formats ];
                      nixpkgs.hostPlatform = "aarch64-linux";
                    }
                    ./hosts/nixos/dev
                  ];
              };

            };

        };
      }
    );
}
