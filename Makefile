UNAME := $(shell uname)
UNAME_P := $(shell uname -p)
ifeq ($(UNAME_P),arm)
BOOTSTRAP := bootstrap-arm
else
BOOTSTRAP := bootstrap-x86
endif

# Channels
NIX_CHANNELS := nixpkgs nixos-stable nixpkgs-stable-darwin
HOME_CHANNELS := home-manager darwin
EMACS_CHANNELS := emacs-overlay
SPACEMACS_CHANNELS := spacemacs
DOOM_CHANNELS := doomemacs
ZSH_CHANNELS := fast-syntax-highlighting fzf-tab powerlevel10k
MISC_CHANNELS := android-nixpkgs flake-utils flake-compat


ifeq ($(UNAME), Darwin) # darwin rules
all:
	@echo "switch.osx_bootstrap"
	@echo "switch.macbook"
	@echo "switch.bot"

switch.bootstrap: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch  --verbose --flake ".#$(BOOTSTRAP)"
switch.macbook: result/sw/bin/darwin-rebuild
	TERM=xterm ./result/sw/bin/darwin-rebuild switch --verbose --flake .#macbook
switch.bot: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch --verbose --flake .#bot

result/sw/bin/darwin-rebuild:
	nix build ".#darwinConfigurations.$(BOOTSTRAP).system"

endif # end osx


ifeq ($(UNAME), Linux) # linux rules

all:
	@echo "switch.cloud"

switch.cloud:
	nix build .#homeConfigurations.cloud.activationPackage
	./result/activate switch --verbose; ./result/activate

endif # end linux

clean:
	./result/sw/bin/nix-collect-garbage

fclean:
	@echo "/!\ require to be root"
	sudo ./result/sw/bin/nix-env -p /nix/var/nix/profiles/system --delete-generations old
	./result/sw/bin/nix-collect-garbage -d
# Remove entries from /boot/loader/entries:
	sudo bash -c "cd /boot/loader/entries; ls | grep -v <current-generation-name> | xargs rm"


fast-update: update.nix update.zsh update.misc # fast update ignore emacs update
update: update.nix update.emacs update.spacemacs update.zsh update.misc
update.nix:; nix flake lock $(addprefix --update-input , $(NIX_CHANNELS))
update.emacs:; nix flake lock $(addprefix --update-input , $(EMACS_CHANNELS))
update.spacemacs:; nix flake lock $(addprefix --update-input , $(SPACEMACS_CHANNELS))
update.doom:; nix flake lock $(addprefix --update-input , $(DOOM_CHANNELS))
update.zsh:; nix flake lock $(addprefix --update-input ,$(ZSH_CHANNELS))
update.misc:; nix flake lock $(addprefix --update-input ,$(MISC_CHANNELS))
update.home:; nix flake lock $(addprefix --update-input , $(NIX_CHANNELS))
