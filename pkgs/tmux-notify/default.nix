{ lib, tmuxPlugins, fetchFromGitHub }:

tmuxPlugins.mkTmuxPlugin {
  pluginName = "tmux-notify";
  version = "1.6.0";
  
  src = fetchFromGitHub {
    owner = "rickstaa";
    repo = "tmux-notify";
    rev = "b713320af05837c3b44e4d51167ff3062dbeae4b";
    sha256 = "05j3r54ispbjbqc5lf3yf9hi7w10znkj4vmlc9k02p2nrgdamsf0";
  };

  rtpFilePath = "tnotify.tmux";

  postInstall = ''
    # Make scripts executable
    chmod +x $out/share/tmux-plugins/tmux-notify/scripts/*.sh
    chmod +x $out/share/tmux-plugins/tmux-notify/tnotify.tmux
  '';

  meta = with lib; {
    homepage = "https://github.com/rickstaa/tmux-notify";
    description = "A tmux plugin that enables you to receive notifications when processes complete";
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [];
  };
}