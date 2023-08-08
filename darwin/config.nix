{ config, lib, pkgs, ... }:

let source = "${lib.cleanSource ../config/yabai}";
in {
  services.skhd = {
    enable = true;
    skhdConfig = ''
      # open terminal
      cmd - return : ~/Applications/kitty.app/Contents/MacOS/kitty --single-instance -d ~
      cmd - 0x18 : /Applications/Firefox.app/Contents/MacOS/firefox # cmd + =

      ## Window command

      # focus
      lcmd - left : ${pkgs.yabai} -m window --focus west
      lcmd - down : ${pkgs.yabai} -m window --focus south
      lcmd - up : ${pkgs.yabai} -m window --focus north
      lcmd - right : ${pkgs.yabai} -m window --focus east

      # swap
      lcmd + shift - left : ${pkgs.yabai} -m window --swap west
      lcmd + shift - down : ${pkgs.yabai} -m window --swap south
      lcmd + shift - up : ${pkgs.yabai} -m window --swap north
      lcmd + shift - right : ${pkgs.yabai} -m window --swap east

      # warp
      lcmd + alt - left : ${pkgs.yabai} -m window --warp west
      lcmd + alt - down : ${pkgs.yabai} -m window --warp south
      lcmd + alt - up : ${pkgs.yabai} -m window --warp north
      lcmd + alt - right : ${pkgs.yabai} -m window --warp east

      # fullscreen
      lcmd + shift - f : ${pkgs.yabai} -m window --toggle native-fullscreen

      # equalize window
      lcmd + shift - e : ${pkgs.yabai} -m space --balance

      # mirror x and y
      lcmd + shift - x: ${pkgs.yabai} -m space --mirror x-axis
      lcmd + shift - y: ${pkgs.yabai} -m space --mirror y-axis

      # toggle bsp
      lcmd + shift - b : ${pkgs.yabai} -m window --toggle float || ${pkgs.yabai} -m window --toggle bsp

      # toggle split
      lcmd + shift - s : ${pkgs.yabai} -m window --toggle split

      # emacs client
      # @TODO: use nix abs path
      # @TODO: use current term directory
      cmd + shift - o : emacsclient --create-frame
    '';
  };

  services.yabai = {
    enable = true;
    enableScriptingAddition = false;
    config = {
      layout = "bsp";
      auto_balance = "on";

      mouse_modifier = "fn";
      mouse_action1 = "resize";
      mouse_action2 = "move";

      top_padding = 30;
      bottom_padding = 5;
      left_padding = 5;
      right_padding = 5;
      window_gap = 10;

      window_topmost = "on";
    };

    extraConfig = ''
      yabai -m rule --add app="^(Calculator|Software Update|Dictionary|VLC|System Preferences|System Settings|zoom.us|Photo Booth|Archive Utility|Python|LibreOffice|App Store|Steam|Alfred|Activity Monitor)$" manage=off
      yabai -m rule --add label="Finder" app="^Finder$" title="(Co(py|nnect)|Move|Info|Pref)" manage=off
      yabai -m rule --add label="Safari" app="^Safari$" title="^(General|(Tab|Password|Website|Extension)s|AutoFill|Se(arch|curity)|Privacy|Advance)$" manage=off
      yabai -m rule --add label="About This Mac" app="System Information" title="About This Mac" manage=off
      yabai -m rule --add app="emacs" manage=on
    '';
  };
}
