UNAME := $(shell uname)

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

