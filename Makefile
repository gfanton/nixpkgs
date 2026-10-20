SHELL := /bin/sh
.DEFAULT_GOAL := help

NIX := nix --extra-experimental-features 'nix-command flakes'
NIX_FLAGS ?= --fallback
# Inputs to update; empty updates every input.
INPUTS ?=

UNAME := $(shell uname)
DARWIN_HOSTS := tzatziki kalamata

.PHONY: help update check fmt hooks clean gc

help: ## List the targets
	@awk -F '## ' '/^[^#\t].*:.*## / { n = split($$1, f, ":"); t = n > 2 ? f[2] : f[1]; \
		gsub(/ /, "", t); sub(/%/, "<host>", t); printf "  %-20s %s\n", t, $$2 }' $(MAKEFILE_LIST)

update: ## Update flake inputs (all, or INPUTS="a b")
	$(NIX) flake update $(INPUTS)

check: ## Evaluate and check the flake
	$(NIX) flake check $(NIX_FLAGS)

fmt: ## Format the tracked Nix files
	$(NIX) run --inputs-from . nixpkgs-unstable#nixfmt -- $$(git ls-files '*.nix')

hooks: ## Run gitleaks on each commit
	git config core.hooksPath .githooks

clean: ## Remove the result link
	rm -f result

gc: ## Delete old generations and collect garbage
	sudo nix-collect-garbage -d
	nix-collect-garbage -d

ifeq ($(UNAME),Darwin)
.PHONY: bootstrap $(addprefix build.,$(DARWIN_HOSTS)) $(addprefix switch.,$(DARWIN_HOSTS)) $(addprefix check-host.,$(DARWIN_HOSTS))

bootstrap: ## Install nix-darwin on a new Mac
	$(NIX) build $(NIX_FLAGS) .#darwinConfigurations.bootstrap.system
	sudo ./result/sw/bin/darwin-rebuild switch $(NIX_FLAGS) --flake .#bootstrap

$(addprefix build.,$(DARWIN_HOSTS)): build.%: ## Build a Mac configuration
	darwin-rebuild build $(NIX_FLAGS) --flake .#$*

$(addprefix switch.,$(DARWIN_HOSTS)): switch.%: check-host.% ## Build and activate a Mac configuration
	sudo /run/current-system/sw/bin/darwin-rebuild switch $(NIX_FLAGS) --flake .#$*

$(addprefix check-host.,$(DARWIN_HOSTS)): check-host.%:
	current="$$(scutil --get LocalHostName)" && \
	case " $(DARWIN_HOSTS) " in \
	*" $$current "*) test "$$current" = "$*" || { echo "error: this Mac is $$current, not $*" >&2; exit 1; } ;; \
	esac
endif

ifeq ($(UNAME),Linux)
.PHONY: build.cloud switch.cloud

CLOUD_TARGET := $(if $(filter aarch64,$(shell uname -m)),cloud-arm,cloud-x86)

build.cloud: ## Build the home-manager configuration
	$(NIX) run --inputs-from . home-manager -- build $(NIX_FLAGS) --flake .#$(CLOUD_TARGET)

switch.cloud: ## Build and activate the home-manager configuration
	$(NIX) run --inputs-from . home-manager -- switch $(NIX_FLAGS) --flake .#$(CLOUD_TARGET)
endif
