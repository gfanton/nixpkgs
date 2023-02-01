{ config, lib, pkgs, ... }:

let source = "${lib.cleanSource ../config/yabai}";
in {
  # system.activationScripts.extraActivation.text = ''
  #   # yabai config
  #   echo >&2 "settup yabai config in $HOME/.config/yabai/yabairc"
  #   mkdir -p "$HOME/.config/yabai"
  #   cat "${source}/yabairc" > "$HOME/.config/yabai/yabairc"
  #   chmod +x "$HOME/.config/yabai/yabairc"
  # '';

  #   services.yabai.enable = true;
}
