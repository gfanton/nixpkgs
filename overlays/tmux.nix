final: super:
let
  inherit (super) pkgs lib;
in
{
  tmuxPlugins = super.tmuxPlugins // {
    tmux-notify = pkgs.callPackage ../pkgs/tmux-notify { };
    tmux-open-emacs = pkgs.callPackage ../pkgs/tmux-open-emacs {
      # Pass xemacsclient from the current context if available
      xemacsclient = if (super ? xemacsclient) then super.xemacsclient else null;
    };
  };
}
