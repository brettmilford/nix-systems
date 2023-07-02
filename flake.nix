{
  description = "Nix systems config";

  nixConfig.bash-prompt = "\[nix-develop\]$ ";

  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
      darwin.url = "github:lnl7/nix-darwin";
      darwin.inputs.nixpkgs.follows = "nixpkgs";
      home-manager.url = "github:nix-community/home-manager/release-22.11";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";
      flake-utils.url = "github:numtide/flake-utils";
      agenix.url = "github:ryantm/agenix";
      agenix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    darwin,
    home-manager,
    flake-utils,
    agenix,
  }@inputs:
  let
    nixpkgsConfig = with inputs; {
      config = {
        #allowUnfree = true;
      };
    };
    homeManagerCommonConfig = with self.homeManagerModules; {
      home.stateVersion = "22.11";
      imports = [
        ./home
      ];
    };
    nixDarwinCommonModules = { user }: [
      {
        system.stateVersion = 4;
        nix = {
          extraOptions = ''
            extra-platforms = aarch64-darwin x86_64-darwin
            experimental-features = nix-command flakes
          '';
        };
      }
      home-manager.darwinModules.home-manager
      {
        users.users.${user}.home = "/Users/${user}";
        home-manager.useGlobalPkgs = true;
        home-manager.users.${user} = homeManagerCommonConfig;
      }
      agenix.darwinModules.default
    ];
  in {
    darwinConfigurations."thamrys" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = {
        inherit inputs nixpkgs;
        pkgs_x86 = import nixpkgs { system = "x86_64-darwin"; };
      };
      modules = nixDarwinCommonModules { user = "brett"; } ++ [
        ./hosts/darwin/thamrys
      ];
    };
  } //
  flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        buildInputs = [
          agenix.packages.${system}.default
        ];
        devShell = import ./shell.nix { inherit pkgs; };
      }
    );
}
