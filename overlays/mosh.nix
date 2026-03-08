final: prev: {
  # Use jdrouhard's patched mosh fork with working OSC 52 clipboard support.
  # Stock mosh 1.4.0 drops OSC 52 sequences from tmux (selection parameter bug).
  # PR #1104 (https://github.com/mobile-shell/mosh/pull/1104) fixes this but is unmerged.
  mosh = prev.mosh.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "jdrouhard";
      repo = "mosh";
      rev = "3d613c845cae0b8966a5d5dbadf2639a9e2f6fd8";
      hash = "sha256-I0YlND+B7MigFKQg+nnTFQb/li+D5oe/CFwoAc9eODg=";
    };
    # Drop patches already applied in the fork
    patches = builtins.filter
      (p: !prev.lib.hasSuffix "eee1a8cf413051c2a9104e8158e699028ff56b26.patch" (toString p))
      (old.patches or [ ]);
  });
}
