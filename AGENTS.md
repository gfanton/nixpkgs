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

## Before every commit and push

This repository is public. Before each commit and each push, read the staged
diff and the commit message as a stranger on GitHub would, and remove anything
that exposes the user or the user's network:

- Credentials: API keys, tokens, private keys, passwords, cookies, session
  links.
- Network details: public IPs, specific private IPs, MAC addresses, network,
  VLAN, VPN and tailnet names, private service domains, and host names other
  than the machines this flake defines.
- Personal details: email addresses other than the commit identity, account
  names, employer, other accounts, location.

Generic values are fine: RFC1918 ranges, default ports, public SSH keys, and
the host names this flake already defines. Machine-specific values go in
untracked files (such as `~/.ssh/config.local`) or in a private repository. Any
hit blocks the commit, and the check is never weakened to make it pass.

`make hooks` turns on the hooks in `.githooks/`, which run gitleaks on each
staged diff and commit message. Gitleaks finds credentials, email addresses, MAC
addresses and tailnet names. It cannot know host, network or service names, so
the read above still applies. Remove a finding from the change. Never bypass it
with `--no-verify`, a `gitleaks:allow` comment or a `.gitleaksignore` entry
unless the user approves.
