{
  inputs = {
    # Package sets
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Environment/system management
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # flake utils
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    flake-utils.url = "github:numtide/flake-utils";

    # overlay
    home-manager.url = "github:nix-community/home-manager/master";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Other sources

    # emacs
    spacemacs.url = "github:syl20bnr/spacemacs/develop";
    spacemacs.flake = false;

    doomemacs.url = "github:doomemacs/doomemacs/master";
    doomemacs.flake = false;

    chemacs2.url = "github:plexus/chemacs2/main";
    chemacs2.flake = false;

    # yabai
    # yabai.url = "github:koekeishiya/yabai";
    # yabai.flake = false;

    # zsh plugins
    fast-syntax-highlighting.url = "github:zdharma-continuum/fast-syntax-highlighting";
    fast-syntax-highlighting.flake = false;

    fzf-tab.url = "github:Aloxaf/fzf-tab";
    fzf-tab.flake = false;

    powerlevel10k.url = "github:romkatv/powerlevel10k";
    powerlevel10k.flake = false;
  };

  outputs =
    {
      self,
      darwin,
      home-manager,
      flake-utils,
      ...
    }@inputs:
    let
      inherit (self.lib)
        attrValues
        makeOverridable
        mkForce
        optionalAttrs
        singleton
        ;

      homeStateVersion = "24.11";

      # Configuration for `nixpkgs`
      nixpkgsDefaults = {
        config = {
          allowUnfree = true;
        };
        overlays = attrValues self.overlays ++ [
          # put stuff here
        ];
      };

      primaryUserInfo = {
        username = "gfanton";
        fullName = "";
        email = "8671905+gfanton@users.noreply.github.com";
        nixConfigDirectory = "/Users/gfanton/nixpkgs";
      };

      ciUserInfo = {
        username = "runner";
        fullName = "";
        email = "github-actions@github.com";
        nixConfigDirectory = "/Users/runner/work/nixpkgs/nixpkgs";
      };
    in
    {

      # Add some additional functions to `lib`.
      lib = inputs.nixpkgs-unstable.lib.extend (
        _: _: {
          mkDarwinSystem = import ./lib/mkDarwinSystem.nix inputs;
          lsnix = import ./lib/lsnix.nix;
        }
      );

      overlays = {
        # Overlays to add different versions `nixpkgs` into package set
        pkgs-master = _: prev: {
          pkgs-master = import inputs.nixpkgs-master {
            inherit (prev.stdenv) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-stable = _: prev: {
          pkgs-stable = import inputs.nixpkgs-stable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-unstable = _: prev: {
          pkgs-unstable = import inputs.nixpkgs-unstable {
            inherit (prev.stdenv) system;
            inherit (nixpkgsDefaults) config;
          };
        };

        # Overlay useful on Macs with Apple Silicon
        pkgs-silicon =
          _: prev:
          optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
            # Add access to x86 packages system is running Apple Silicon
            pkgs-x86 = import inputs.nixpkgs-unstable {
              system = "x86_64-darwin";
              inherit (nixpkgsDefaults) config;
            };
          };

        # non flake inputs
        my-inputs = final: prev: {
          spacemacs = inputs.spacemacs;
          doomemacs = inputs.doomemacs;
          chemacs2 = inputs.chemacs2;
          zsh-plugins.fast-syntax-highlighting = inputs.fast-syntax-highlighting;
          zsh-plugins.fzf-tab = inputs.fzf-tab;
          zsh-plugins.powerlevel10k = inputs.powerlevel10k;
          # yabai = inputs.yabai;
        };

        # My overlays
        my-loon = import ./overlays/loon.nix;
        my-gnolint = import ./overlays/gnolint.nix;
        my-libvterm = import ./overlays/libvterm.nix;
      };

      # Non-system outputs --------------------------------------------------------------------- {{{

      commonModules = {
        colors = import ./modules/home/colors;

        my-colors = import ./home/colors.nix;
      };

      darwinModules = {
        # My configurations
        my-bootstrap = import ./darwin/bootstrap.nix;
        my-defaults = import ./darwin/defaults.nix;
        my-env = import ./darwin/env.nix;
        my-homebrew = import ./darwin/homebrew.nix;
        my-yabai = import ./darwin/yabai.nix;
        my-jankyborders = import ./darwin/jankyborders.nix;
        my-skhd = import ./darwin/skhd.nix;

        # local modules
        services-emacsd = import ./modules/darwin/services/emacsd.nix;
        users-primaryUser = import ./modules/darwin/users.nix;
        programs-nix-index = import ./modules/darwin/programs/nix-index.nix;
      };

      homeManagerModules = {
        # My configurations
        my-shells = import ./home/shells.nix;
        my-git = import ./home/git.nix;
        my-kitty = import ./home/kitty.nix;
        my-packages = import ./home/packages.nix;
        my-asdf = import ./home/asdf.nix;
        my-emacs = import ./home/emacs.nix;
        my-config = import ./home/config.nix;

        # local modules
        programs-truecolor = import ./modules/home/programs/truecolor;
        # programs-asdf = import ./modules/home/programs/asdf;
        programs-kitty-extras = import ./modules/home/programs/kitty/extras.nix;
        programs-zsh-oh-my-zsh-extra = import ./modules/home/programs/zsh/oh-my-zsh/extras.nix;

        home-user-info =
          { lib, ... }:
          {
            options.home.user-info =
              (self.darwinModules.users-primaryUser { inherit lib; }).options.users.primaryUser;
          };
      };
      # }}}

      # System outputs ------------------------------------------------------------------------- {{{

      # My `nix-darwin` configs
      darwinConfigurations = rec {
        # Mininal configurations to bootstrap systems
        bootstrap-x86 = makeOverridable darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          modules = [
            ./darwin/bootstrap.nix
            { nixpkgs = nixpkgsDefaults; }
          ];
        };
        bootstrap-arm = bootstrap-x86.override { system = "aarch64-darwin"; };

        # My Apple Silicon macOS laptop config
        macbook = makeOverridable self.lib.mkDarwinSystem (
          primaryUserInfo
          // {
            system = "aarch64-darwin";
            modules =
              (attrValues self.darwinModules)
              ++ (attrValues self.commonModules)
              ++ singleton {
                nixpkgs = nixpkgsDefaults;
                networking.computerName = "guicp";
                networking.hostName = "ghost";
                networking.knownNetworkServices = [
                  "Wi-Fi"
                  "USB 10/100/1000 LAN"
                ];
                nix.registry.my.flake = inputs.self;
              };

            inherit homeStateVersion;
            homeModules =
              (attrValues self.homeManagerModules)
              ++ (attrValues self.commonModules)
              ++ [

              ];
          }
        );

        # Config with small modifications needed/desired for CI with GitHub workflow
        githubCI = self.darwinConfigurations.macbook.override {
          system = "aarch64-darwin";
          username = "runner";
          nixConfigDirectory = "/Users/runner/work/nixpkgs/nixpkgs";
          extraModules = singleton {
            environment.etc.shells.enable = mkForce false;
            environment.etc."nix/nix.conf".enable = mkForce false;
            homebrew.enable = mkForce false;
            services.yabai.enable = mkForce false;
            services.skhd.enable = mkForce false;
            ids.uids.nixbld = 300; # [hack] unil macos-15
          };
        };
      };

      # Config I use with non-NixOS Linux systems (e.g., cloud VMs etc.)
      # Build and activate on new system with:
      # `nix build .#homeConfigurations.cloud.activationPackage && ./result/activate`
      homeConfigurations = {
        cloud = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "x86_64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, ... }:
              {
                home.user-info = primaryUserInfo // {
                  nixConfigDirectory = "${config.home.homeDirectory}/nixpkgs";
                };
                home.username = config.home.user-info.username;
                home.homeDirectory = "/home/${config.home.username}";
                home.stateVersion = homeStateVersion;
              }
            );
        };

        # specific config for github ci
        githubCI = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "x86_64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, ... }:
              {
                home.user-info = ciUserInfo // {
                  nixConfigDirectory = "${config.home.homeDirectory}/nixpkgs";
                };
                home.username = config.home.user-info.username;
                home.homeDirectory = "/home/${config.home.username}";
                home.stateVersion = homeStateVersion;
              }
            );
        };
      };
      # }}}

      # Add re-export `nixpkgs` packages with overlays.
      # This is handy in combination with `nix registry add my /Users/gfanton/nixpkgs`
    }
    // flake-utils.lib.eachDefaultSystem (system: {
      # Re-export `nixpkgs-unstable` with overlays.
      # This is handy in combination with setting `nix.registry.my.flake = inputs.self`.
      # Allows doing things like `nix run my#prefmanager -- watch --all`
      legacyPackages = import inputs.nixpkgs-unstable (nixpkgsDefaults // { inherit system; });

      # Development shells ----------------------------------------------------------------------{{{
      # Shell environments for development
      # With `nix.registry.my.flake = inputs.self`, development shells can be created by running,
      # e.g., `nix develop my#python`.
      devShells =
        let
          pkgs = self.legacyPackages.${system};
        in
        {
          asdf = pkgs.mkShell {
            name = "asdf";
            inputsFrom = attrValues { inherit (pkgs) asdf-vm; };
            shellHook = ''
              if [ -f "${pkgs.asdf-vm}/share/asdf-vm/asdf.sh" ]; then
                . "${pkgs.asdf-vm}/share/asdf-vm/asdf.sh"
              fi

              fpath=(${pkgs.asdf-vm}/share/asdf-vm/completions $fpath)

              if [ -f "''${ASDF_DATA_DIR}/.asdf/plugins/java/set-java-home.zsh" ]; then
                 . "''${ASDF_DATA_DIR}/.asdf/plugins/java/set-java-home.zsh"
              fi
            '';
          };
        };
      # }}}
    });
}
