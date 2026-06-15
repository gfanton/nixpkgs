final: super:
let
  inherit (super) pkgs;
in
{
  gnotify = pkgs.callPackage ../pkgs/gnotify { };
}
