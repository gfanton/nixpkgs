# nixpkgs

Personal Nix flake for a Darwin laptop and some Linux VMs. Everything here is
public.

## Claude Code configuration lives elsewhere

Claude Code's configuration is not in this repository. It is maintained in a
separate private repository, wired in two ways:

- `config/claude-config/` — a git submodule. Cloning this repo without access to
  it leaves the directory empty, and every Nix expression below still evaluates.
- `claude-config` in `flake.nix` — a flake input, `flake = false`. This is what
  `home/claude-code.nix` actually reads to build the deployed configuration; the
  submodule is the working copy for editing it.

The two are pinned independently, so the gitlink and `flake.lock` can disagree.
`flake.lock` decides what gets built.

Do not copy configuration out of the submodule into this repository, and do not
describe its contents in files here, in commit messages, or in a pull request.
Anything that needs to be public belongs in `home/claude-code.nix`, which holds
only the wiring.

## Layout

- `flake.nix` — inputs and the Darwin/home-manager outputs
- `home/` — home-manager modules, one per concern
- `darwin/` — macOS system configuration
- `overlays/`, `pkgs/` — package overrides and locally packaged software
- `config/` — configuration trees, including the submodule above
