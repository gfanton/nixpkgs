final: super:
let
  inherit (super) pkgs lib;
in
{
  tmuxPlugins = super.tmuxPlugins // {
    tmux-notify = pkgs.callPackage ../pkgs/tmux-notify { };
  };
}
