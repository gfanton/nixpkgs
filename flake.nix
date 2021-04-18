{
  description = "gfanton dotfiles";

  inputs = {
    # channel
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
    nixpkgs-master = { url = "github:nixos/nixpkgs/master"; };
    nixpkgs-stable-darwin = { url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin"; };
    nixpkgs-silicon-darwin = { url = "github:thefloweringash/nixpkgs/apple-silicon"; };
    nixos-stable = { url = "github:nixos/nixpkgs/nixos-20.09"; };

    # flake
    flake-utils = { url = "github:numtide/flake-utils"; };
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };

    darwin = { url = "github:LnL7/nix-darwin"; inputs.nixpkgs.follows = "nixpkgs"; };
    home-manager = { url = "github:nix-community/home-manager"; inputs.nixpkgs.follows = "nixpkgs"; };

    # Other sources
    spacemacs = { url = "github:syl20bnr/spacemacs/develop"; flake = false; };
  };

  outputs = { self, nixpkgs, darwin, home-manager, flake-utils, ... }@inputs:
    let
      nixpkgsConfig = { mysystem }: with inputs; {
        config = {
          allowUnfree = true;
        };
        overlays = self.overlays ++ [
          (
            final: prev:
              let
                # system = prev.stdenv.system ;
                system = if mysystem == "aarch64-darwin" then "x86_64-darwin" else mysystem;
                nixpkgs-stable = if system == "x86_64-darwin" then nixpkgs-stable-darwin else nixos-stable;
                nixpkgs-silicon = if system == "x86_64-darwin" then nixpkgs-silicon-darwin else nixos-stable;
              in
              {
                master = nixpkgs-master.legacyPackages.${system};
                stable = nixpkgs-stable.legacyPackages.${system};
                silicon = nixpkgs-silicon.legacyPackages.${mysystem};
              }
          )
        ];
      };

      homeManagerConfig = with self.homeManagerModules; {
          imports = [
            ./home
            misc.truecolor
            programs.mykitty
            programs.kitty.extras
          ];
      };

      nixDarwinCommonModules = { system, user }: [
        # Include extra `nix-darwin`
        self.darwinModules.services.emacsd
        self.darwinModules.security.pam
        # Main `nix-darwin` config
        ./darwin
        # `home-manager` module
        home-manager.darwinModules.home-manager
        {
          nixpkgs = nixpkgsConfig { mysystem = system; };
          # Hack to support legacy worklows that use `<nixpkgs>` etc.
          nix.nixPath = { nixpkgs = "$HOME/nixpkgs/nixpkgs.nix"; };
          # `home-manager` config
          users.users.${user}.home = "/Users/${user}";
          home-manager.useGlobalPkgs = true;
          home-manager.users.${user} = homeManagerConfig;
        }
      ];

    in
    {
      darwinConfigurations = {

        # Minimal configuration to bootstrap systems
        bootstrap = darwin.lib.darwinSystem {
          modules = [ ./darwin/bootstrap.nix {
            nixpkgs = nixpkgsConfig { mysystem = "x86_64-darwin"; };
          } ];
        };

        macbook = darwin.lib.darwinSystem {
          modules = nixDarwinCommonModules { system = "aarch64-darwin"; user = "gfanton"; } ++ [
            {
              networking.computerName = "guicp";
              networking.hostName = "ghost";
              networking.knownNetworkServices = [
                "Wi-Fi"
                "USB 10/100/1000 LAN"
              ];
            }
          ];
        };

        bot = darwin.lib.darwinSystem {
          modules = nixDarwinCommonModules { system = "x86_64-darwin"; user = "gfantonbot"; } ++ [
            {
              networking.computerName = "guibot";
              networking.hostName = "gbot";
              networking.knownNetworkServices = [
                "Wi-Fi"
                "USB 10/100/1000 LAN"
              ];
            }
          ];
        };
      };

      cloud = home-manager.lib.homeManagerConfiguration {
	      system = "x86_64-linux";
	      homeDirectory = "/home/gfanton";
	      username = "gfanton";
	      configuration = {
	        imports = [ homeManagerConfig ];
	        nixpkgs = nixpkgsConfig { mysystem = "x86_64-linux"; };
	      };
      };

      darwinModules = {
        security.pam = import ./darwin/modules/security/pam.nix;
        services.emacsd = import ./darwin/modules/services/emacsd.nix;
      };

      homeManagerModules = {
        misc.truecolor = import ./home/modules/misc/truecolor.nix;
        programs.kitty.extras = import ./home/modules/programs/kitty/extras.nix;
        programs.mykitty = import ./home/modules/programs/kitty;
      };

      overlays = with inputs; [
      (
        final: prev: {
          # Some packages
          # comma = import comma { inherit (prev) pkgs; };
          spacemacs = inputs.spacemacs;
        }
      )
      # Other overlays that don't depend on flake inputs.
    ] ++ map import ((import ./lsnix.nix) ./overlays);
  } // flake-utils.lib.eachDefaultSystem (system: {
     legacyPackages = import nixpkgs { inherit system; inherit (nixpkgsConfig { mysystem = system; }) config overlays; };
 });
}
