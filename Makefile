IMPURE ?= false
FALLBACK ?= true

UNAME := $(shell uname)

BOOTSTRAP := bootstrap

# The Macs in flake.nix. A switch sets the host name along with everything
# else, so switch.<host> run on another of them turns that Mac into <host>.
DARWIN_HOSTS := tzatziki kalamata

# Channels (matching flake.nix inputs)
NIX_CHANNELS := nixpkgs-master nixpkgs-stable nixpkgs-unstable
HOME_CHANNELS := home-manager darwin
EMACS_CHANNELS := emacs-overlay chemacs2
SPACEMACS_CHANNELS := spacemacs
DOOM_CHANNELS := doomemacs
ZSH_CHANNELS := fast-syntax-highlighting powerlevel10k
MISC_CHANNELS := flake-utils flake-compat project devenv

NIX_FILES := $(shell find . -type f -name '*.nix')

impure := $(if $(filter $(IMPURE),true),--impure,)
fallback := $(if $(filter $(FALLBACK),true),--fallback,)

ifeq ($(UNAME), Darwin) # darwin rules
all:
	@echo "switch.bootstrap"
	@printf 'switch.%s\n' $(DARWIN_HOSTS)

build.tzatziki:
	nix build ${impure} ${fallback} --verbose .#darwinConfigurations.tzatziki.system

build.kalamata:
	nix build ${impure} ${fallback} --verbose .#darwinConfigurations.kalamata.system

build.cloud:
	nix build ${impure} ${fallback} --verbose .#homeConfigurations.$(CLOUD_TARGET).activationPackage

check:
	nix flake check

switch.bootstrap: result/sw/bin/darwin-rebuild
	./result/sw/bin/darwin-rebuild switch ${impure} ${fallback} --verbose --flake ".#$(BOOTSTRAP)"
.PHONY: $(addprefix switch.,$(DARWIN_HOSTS)) $(addprefix check-host.,$(DARWIN_HOSTS))

# Passes on <host> itself, and on a Mac that is none of DARWIN_HOSTS yet, such
# as a new machine still under its factory name.
$(addprefix check-host.,$(DARWIN_HOSTS)): check-host.%:
	current="$$(scutil --get LocalHostName)" && \
	case " $(DARWIN_HOSTS) " in \
	*" $$current "*) test "$$current" = "$*" || { echo "error: this Mac is $$current, not $*" >&2; exit 1; } ;; \
	esac

$(addprefix switch.,$(DARWIN_HOSTS)): switch.%: check-host.% result/sw/bin/darwin-rebuild
	TERM=xterm sudo ./result/sw/bin/darwin-rebuild switch ${impure} ${fallback} --verbose --flake .#$*

result/sw/bin/darwin-rebuild:
	nix --experimental-features 'flakes nix-command' build ".#darwinConfigurations.$(BOOTSTRAP).system"

endif # end osx


ifeq ($(UNAME), Linux) # linux rules

# Detect architecture for Linux
LINUX_ARCH := $(shell uname -m)
ifeq ($(LINUX_ARCH),aarch64)
CLOUD_TARGET := cloud-arm
else
CLOUD_TARGET := cloud-x86
endif

all:
	@echo "switch.cloud"

switch.cloud:
	nix build --extra-experimental-features nix-command --extra-experimental-features flakes .#homeConfigurations.$(CLOUD_TARGET).activationPackage
	./result/activate

endif # end linux

fmt:
	nix-shell -p nixfmt --command "nixfmt  $(NIX_FILES)"

clean:
	./result/sw/bin/nix-collect-garbage

fclean:
	@echo "/!\ require to be root"
	sudo ./result/sw/bin/nix-env -p /nix/var/nix/profiles/system --delete-generations old
	./result/sw/bin/nix-collect-garbage -d
# Remove entries from /boot/loader/entries:


fast-update: update.nix update.zsh update.misc # fast update ignore emacs update
update: update.nix update.home update.emacs update.spacemacs update.doom update.zsh update.misc
update.nix:; nix flake lock $(addprefix --update-input , $(NIX_CHANNELS))
update.emacs:; nix flake lock $(addprefix --update-input , $(EMACS_CHANNELS))
update.spacemacs:; nix flake lock $(addprefix --update-input , $(SPACEMACS_CHANNELS))
update.doom:; nix flake lock $(addprefix --update-input , $(DOOM_CHANNELS))
update.zsh:; nix flake lock $(addprefix --update-input ,$(ZSH_CHANNELS))
update.misc:; nix flake lock $(addprefix --update-input ,$(MISC_CHANNELS))
update.home:; nix flake lock $(addprefix --update-input , $(HOME_CHANNELS))
