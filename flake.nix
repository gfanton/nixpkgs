{
  inputs = {
    # Package sets
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/release-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # herdr 0.8.2 is in nixpkgs master but not in the unstable channel yet, so it
    # gets its own rev rather than dragging every other package forward to reach
    # one binary. Drop this input and take herdr from nixpkgs-unstable once that
    # pin carries 0.8.2.
    nixpkgs-herdr.url = "github:NixOS/nixpkgs/916377a8f81a1cf4834e110f8fbe2666938dbc4b";

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

    # yabai
    # yabai.url = "github:koekeishiya/yabai";
    # yabai.flake = false;

    # zsh plugins
    fast-syntax-highlighting.url = "github:zdharma-continuum/fast-syntax-highlighting";
    fast-syntax-highlighting.flake = false;

    powerlevel10k.url = "github:romkatv/powerlevel10k";
    powerlevel10k.flake = false;

    # My project
    project.url = "github:gfanton/project/v0.19.0";
    project.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # devenv (flake output bundles patched nix that fixes Boehm GC crash on aarch64-darwin)
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # Claude Code config (private). The submodule at config/claude-config is the
    # working copy; this input is what home/claude-code.nix actually builds from,
    # so an edit reaches the system only once it is pushed and relocked.
    claude-config.url = "git+ssh://git@github.com/gfanton/claude-config.git";
    claude-config.flake = false;

    # Claude Code plugins
    claude-plugin-superpowers.url = "github:obra/superpowers/v6.3.0";
    claude-plugin-superpowers.flake = false;

    claude-plugins-official.url = "github:anthropics/claude-plugins-official";
    claude-plugins-official.flake = false;

    # Backlog.md — task tracker (upstream flake)
    backlog-md.url = "github:MrLesk/Backlog.md/v1.50.1";

    # Matt Pocock's Claude Code skills (no release tags — pinned by SHA)
    pocock-skills.url = "github:mattpocock/skills/b8be62ffacb0118fa3eaa29a0923c87c8c11985c";
    pocock-skills.flake = false;
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

      homeStateVersion = "25.11";

      # Configuration for `nixpkgs`
      nixpkgsDefaults = {
        config = {
          allowUnfree = true;
        };
        overlays = attrValues self.overlays ++ [
          # Emacs overlay for latest packages and optimizations
          inputs.emacs-overlay.overlays.default
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

      mkMacHost =
        {
          hostName,
          computerName ? hostName,
          knownNetworkServices ? [ ],
          hostModules ? [ ],
        }:
        makeOverridable self.lib.mkDarwinSystem (
          primaryUserInfo
          // {
            system = "aarch64-darwin";
            modules =
              (attrValues self.darwinModules)
              ++ (attrValues self.commonModules)
              ++ hostModules
              ++ singleton {
                nixpkgs = nixpkgsDefaults;
                networking = { inherit computerName hostName knownNetworkServices; };
                nix.registry.my.flake = inputs.self;
              };

            inherit homeStateVersion;
            homeModules = (attrValues self.homeManagerModules) ++ (attrValues self.commonModules);
          }
        );
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
            inherit (prev.stdenv.hostPlatform) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-stable = _: prev: {
          pkgs-stable = import inputs.nixpkgs-stable {
            inherit (prev.stdenv.hostPlatform) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-unstable = _: prev: {
          pkgs-unstable = import inputs.nixpkgs-unstable {
            inherit (prev.stdenv.hostPlatform) system;
            inherit (nixpkgsDefaults) config;
          };
        };
        pkgs-herdr = _: prev: {
          pkgs-herdr = import inputs.nixpkgs-herdr {
            inherit (prev.stdenv.hostPlatform) system;
            inherit (nixpkgsDefaults) config;
          };
        };

        # non flake inputs
        my-inputs = final: prev: {
          zsh-plugins.fast-syntax-highlighting = inputs.fast-syntax-highlighting;
          # yabai = inputs.yabai;
          project = inputs.project.packages.${final.stdenv.hostPlatform.system}.default;
          # Pre-packaged tmux plugin from project flake (properly wrapped with binaries)
          projectTmuxPlugin = inputs.project.packages.${final.stdenv.hostPlatform.system}.tmux-proj;
          proj-herdr = inputs.project.packages.${final.stdenv.hostPlatform.system}.proj-herdr;
          # devenv from flake (bundles patched nix with Boehm GC fix for aarch64-darwin)
          devenv = inputs.devenv.packages.${final.stdenv.hostPlatform.system}.devenv;
          # Claude Code plugin sources (non-flake inputs for easy nix flake update)
          claude-plugin-superpowers = inputs.claude-plugin-superpowers;
          claude-plugins-official-src = inputs.claude-plugins-official;
          pocock-skills-src = inputs.pocock-skills;
          backlog-md = inputs.backlog-md.packages.${final.stdenv.hostPlatform.system}.default;
        };

        # My overlays
        my-loon = import ./overlays/loon.nix;
        my-libvterm = import ./overlays/libvterm.nix;
        my-rtk = import ./overlays/rtk.nix;
        my-tmux = import ./overlays/tmux.nix;
        my-emacs = import ./overlays/emacs.nix;
        my-mosh = import ./overlays/mosh.nix;
        my-claude-plugins = import ./overlays/claude-plugins.nix;
        my-gnotify = import ./overlays/gnotify.nix;
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
        my-colima = import ./darwin/colima.nix;
        my-openssh = import ./darwin/openssh.nix;

        # local modules
        services-my-emacs = import ./modules/darwin/services/my-emacs.nix;
        services-colima = import ./modules/darwin/services/colima.nix;
        users-primaryUser = import ./modules/darwin/users.nix;
        programs-nix-index = import ./modules/darwin/programs/nix-index.nix;
      };

      nixosModules = {
        emacs = import ./modules/nixos/emacs.nix;
      };

      homeManagerModules = {
        # My configurations
        my-shells = import ./home/shells.nix;
        my-git = import ./home/git.nix;
        my-kitty = import ./home/kitty.nix;
        my-alacritty = import ./home/alacritty.nix;
        my-packages = import ./home/packages.nix;
        my-asdf = import ./home/asdf.nix;
        my-emacs = import ./home/emacs.nix;
        my-config = import ./home/config.nix;
        my-colima = import ./home/colima.nix;
        my-starship = import ./home/starship.nix;
        my-ghostty = import ./home/ghostty.nix;
        my-herdr = import ./home/herdr.nix;
        my-ssh-agent-forwarding = import ./home/ssh-agent-forwarding.nix;
        my-claude-code = import ./home/claude-code.nix inputs.claude-config;
        my-backlog-workflow = import "${inputs.claude-config}/backlog-workflow";

        # local modules
        programs-truecolor = import ./modules/home/programs/truecolor;
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
        # Minimal configuration to bootstrap aarch64-darwin systems
        bootstrap = makeOverridable darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./darwin/bootstrap.nix
            { nixpkgs = nixpkgsDefaults; }
          ];
        };

        # My Apple Silicon macOS laptop config
        tzatziki = mkMacHost {
          hostName = "tzatziki";
          knownNetworkServices = [
            "Wi-Fi"
            "USB 10/100/1000 LAN"
          ];
          hostModules = [ { users.primaryUser.forwardAgentTo = [ "kalamata" ]; } ];
        };

        kalamata = mkMacHost {
          hostName = "kalamata";
          hostModules = [ ./darwin/kalamata.nix ];
        };

        # Config with small modifications needed/desired for CI with GitHub workflow
        githubCI = self.darwinConfigurations.tzatziki.override {
          system = "aarch64-darwin";
          username = "runner";
          nixConfigDirectory = "/Users/runner/work/nixpkgs/nixpkgs";
          extraModules = singleton {
            environment.etc.shells.enable = mkForce false;
            environment.etc."nix/nix.conf".enable = mkForce false;
            homebrew.enable = mkForce false;
            services.yabai.enable = mkForce false;
            services.skhd.enable = mkForce false;
            services.openssh.enable = mkForce false;
            users.primaryUser.forwardAgentTo = mkForce [ ];
            ids.gids.nixbld = 350; # [hack]
          };
        };
      };

      # NixOS configurations for cloud VMs
      # Use with: nixos-rebuild switch --flake ~/nixpkgs#cloud-vm
      nixosConfigurations = {
        cloud-vm = inputs.nixpkgs-unstable.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            # Base system configuration
            ./nixos/cloud-vm/configuration.nix

            # Apply overlays and allow unfree
            {
              nixpkgs.overlays = attrValues self.overlays ++ [
                inputs.emacs-overlay.overlays.default
              ];
              nixpkgs.config.allowUnfree = true;
            }

            # Emacs daemon (uses pkgs.myEmacs from overlay)
            self.nixosModules.emacs

            # Home-manager as NixOS module
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
                users.gfanton = {
                  imports = attrValues self.homeManagerModules ++ attrValues self.commonModules;

                  home.user-info = primaryUserInfo // {
                    nixConfigDirectory = "/home/gfanton/nixpkgs";
                  };
                  home.stateVersion = homeStateVersion;
                };
              };
            }
          ];
        };

        cloud-vm-arm = inputs.nixpkgs-unstable.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            # Base system configuration
            ./nixos/cloud-vm/configuration.nix

            # Apply overlays, allow unfree, override platform
            {
              nixpkgs.overlays = attrValues self.overlays ++ [
                inputs.emacs-overlay.overlays.default
              ];
              nixpkgs.config.allowUnfree = true;
              nixpkgs.hostPlatform = "aarch64-linux";
            }

            # Emacs daemon
            self.nixosModules.emacs

            # Home-manager as NixOS module
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
                users.gfanton = {
                  imports = attrValues self.homeManagerModules ++ attrValues self.commonModules;

                  home.user-info = primaryUserInfo // {
                    nixConfigDirectory = "/home/gfanton/nixpkgs";
                  };
                  home.stateVersion = homeStateVersion;
                };
              };
            }
          ];
        };
      };

      # Config I use with non-NixOS Linux systems (e.g., cloud VMs etc.)
      # Build and activate on new system with:
      # `nix build .#homeConfigurations.cloud.activationPackage && ./result/activate`
      homeConfigurations = {
        cloud-x86 = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "x86_64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, lib, ... }:
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

        cloud-arm = home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs-unstable (nixpkgsDefaults // { system = "aarch64-linux"; });
          modules =
            attrValues self.homeManagerModules
            ++ (attrValues self.commonModules)
            ++ singleton (
              { config, lib, ... }:
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

        # Alias for backward compatibility
        cloud = self.homeConfigurations.cloud-x86;

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
