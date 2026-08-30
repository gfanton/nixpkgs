final: super:
let
  inherit (super) pkgs;
in
{
  # macOS resolves a notification's icon from the posting app's bundle id, not
  # from whichever copy of the binary ran, so the identity has to be settled
  # once for the whole system rather than per caller. Claude Code is what fires
  # these banners (home/claude-code.nix), hence Anthropic's mark.
  #
  # Fetched rather than vendored to keep third-party artwork out of the tree;
  # the pinned hash turns an upstream change into a build failure instead of a
  # silent swap. The package itself defaults to no icon and stays unbranded.
  gnotify = pkgs.callPackage ../pkgs/gnotify {
    icon = pkgs.fetchurl {
      url = "https://avatars.githubusercontent.com/u/76263028";
      hash = "sha256-QSS40DsVTSOv4da5oLm3gTxbQEB9jQOcONb/Oy6SMjk=";
      name = "anthropic-logo.png";
    };
  };
}
