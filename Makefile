UNAME := $(shell uname)

# Channels
NIX_CHANNELS := nixos-stable nixpkgs-silicon-darwin nixpkgs-stable-darwin nixpkgs-master nixpkgs
HOME_CHANNELS := home-manager darwin
EMACS_CHANNELS := spacemacs emacs-overlay
ZSH_CHANNELS := fast-syntax-highlighting fzf-tab powerlevel10k
MISC_CHANNELS := android-nixpkgs flake-utils flake-compat


ifeq ($(UNAME), Darwin) # darwin rules
all:
	@echo "switch.osx_bootstrap"
	@echo "switch.macbook"
	@echo "switch.bot"

switch.osx_bootstrap: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch  --verbose --flake .#bootstrap
switch.macbook: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch --verbose --flake .#macbook
switch.bot: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch --verbose --flake .#bot

result/sw/bin/darwin-rebuild:
	nix build .#darwinConfigurations.bootstrap.system

endif # end osx


ifeq ($(UNAME), Linux) # linux rules

all:
	@echo "switch.cloud"

switch.cloud:
		nix build .#cloud.activationPackage
	./result/activate switch --verbose --flake .#bot

endif # end linux

fast-update: update.nix update.zsh update.misc # fast update ignore emacs update
update: update.nix update.emacs update.zsh update.misc
update.nix:; nix flake lock $(addprefix --update-input , $(NIX_CHANNELS))
update.emacs:; nix flake lock $(addprefix --update-input , $(EMACS_CHANNELS))
update.zsh:; nix flake lock $(addprefix --update-input ,$(ZSH_CHANNELS))
update.misc:; nix flake lock $(addprefix --update-input ,$(MISC_CHANNELS))
update.home:; nix flake lock $(addprefix --update-input , $(NIX_CHANNELS))

