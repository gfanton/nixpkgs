IMPURE ?= false
FALLBACK ?= true

UNAME := $(shell uname)
UNAME_P := $(shell uname -p)

ifeq ($(UNAME_P),arm)
BOOTSTRAP := bootstrap-arm
else
BOOTSTRAP := bootstrap-x86
endif

# Channels (matching flake.nix inputs)
NIX_CHANNELS := nixpkgs-master nixpkgs-stable nixpkgs-unstable
HOME_CHANNELS := home-manager darwin
EMACS_CHANNELS := emacs-overlay chemacs2
SPACEMACS_CHANNELS := spacemacs
DOOM_CHANNELS := doomemacs
ZSH_CHANNELS := fast-syntax-highlighting powerlevel10k
MISC_CHANNELS := flake-utils flake-compat project

NIX_FILES := $(shell find . -type f -name '*.nix')

impure := $(if $(filter $(IMPURE),true),--impure,)
fallback := $(if $(filter $(FALLBACK),true),--fallback,)

ifeq ($(UNAME), Darwin) # darwin rules
all:
	@echo "switch.bootstrap"
	@echo "switch.macbook"

build.macbook:
	nix build ${impure} ${fallback} --verbose .#darwinConfigurations.macbook.system

build.cloud:
	nix build ${impure} ${fallback} --verbose .#homeConfigurations.$(CLOUD_TARGET).activationPackage

check:
	nix flake check

switch.bootstrap: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch ${impure} ${fallback} --verbose --flake ".#$(BOOTSTRAP)"
switch.macbook: result/sw/bin/darwin-rebuild
	TERM=xterm sudo ./result/sw/bin/darwin-rebuild switch ${impure} ${fallback} --verbose --flake .#macbook

result/sw/bin/darwin-rebuild:
	nix --experimental-features 'flakes nix-command' build ".#darwinConfigurations.$(BOOTSTRAP).system"

endif # end osx


ifeq ($(UNAME), Linux) # linux rules

# Detect architecture for Linux
LINUX_ARCH := $(shell uname -m)
ifeq ($(LINUX_ARCH),aarch64)
CLOUD_TARGET := cloud-arm
NIXOS_TARGET := cloud-arm
else
CLOUD_TARGET := cloud-x86
NIXOS_TARGET := cloud-x86
endif

all:
	@echo "Available targets:"
	@echo "  switch.cloud       - Home Manager cloud configuration"
	@echo "  build.nixos        - Build NixOS cloud configuration"
	@echo "  build.nixos-lvm    - Build NixOS LVM configuration"
	@echo "  install.nixos      - Install NixOS using nixos-anywhere (requires TARGET_HOST)"
	@echo "  install.nixos-lvm  - Install NixOS LVM using nixos-anywhere (requires TARGET_HOST)"
	@echo "  install.script     - Run automated installation script"

# Home Manager configuration (existing)
switch.cloud:
	nix build --extra-experimental-features nix-command --extra-experimental-features flakes .#homeConfigurations.$(CLOUD_TARGET).activationPackage
	./result/activate

# NixOS configuration builds
build.nixos:
	nix build ${impure} ${fallback} --verbose .#nixosConfigurations.$(NIXOS_TARGET).config.system.build.toplevel

build.nixos-lvm:
	nix build ${impure} ${fallback} --verbose .#nixosConfigurations.cloud-lvm-$(NIXOS_TARGET).config.system.build.toplevel

# NixOS installation using nixos-anywhere (requires TARGET_HOST environment variable)
install.nixos:
	@if [ -z "$(TARGET_HOST)" ]; then \
		echo "Error: TARGET_HOST environment variable is required"; \
		echo "Example: TARGET_HOST=root@192.168.1.100 make install.nixos"; \
		exit 1; \
	fi
	nixos-anywhere --flake .#$(NIXOS_TARGET) $(TARGET_HOST)

install.nixos-lvm:
	@if [ -z "$(TARGET_HOST)" ]; then \
		echo "Error: TARGET_HOST environment variable is required"; \
		echo "Example: TARGET_HOST=root@192.168.1.100 make install.nixos-lvm"; \
		exit 1; \
	fi
	nixos-anywhere --flake .#cloud-lvm-$(NIXOS_TARGET) $(TARGET_HOST)

# Easy deployment script
install.script:
	@echo "Running NixOS installation script..."
	@if [ -f "./install.sh" ]; then \
		chmod +x ./install.sh && ./install.sh $(ARGS); \
	else \
		echo "Error: install.sh not found"; \
		exit 1; \
	fi

endif # end linux

fmt:
	nix-shell -p nixfmt-rfc-style --command "nixfmt  $(NIX_FILES)"

clean:
	./result/sw/bin/nix-collect-garbage

fclean:
	@echo "/!\ require to be root"
	sudo ./result/sw/bin/nix-env -p /nix/var/nix/profiles/system --delete-generations old
	./result/sw/bin/nix-collect-garbage -d
# Remove entries from /boot/loader/entries:


fast-update: update.nix update.zsh update.misc # fast update ignore emacs update
update: update.nix update.home update.emacs update.spacemacs update.zsh update.misc
update.nix:; nix flake lock $(addprefix --update-input , $(NIX_CHANNELS))
update.emacs:; nix flake lock $(addprefix --update-input , $(EMACS_CHANNELS))
update.spacemacs:; nix flake lock $(addprefix --update-input , $(SPACEMACS_CHANNELS))
update.doom:; nix flake lock $(addprefix --update-input , $(DOOM_CHANNELS))
update.zsh:; nix flake lock $(addprefix --update-input ,$(ZSH_CHANNELS))
update.misc:; nix flake lock $(addprefix --update-input ,$(MISC_CHANNELS))
update.home:; nix flake lock $(addprefix --update-input , $(HOME_CHANNELS))
