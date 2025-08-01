{
  lib,
  tmuxPlugins,
  makeWrapper,
  tmux,
  xemacsclient ? null,
}:

tmuxPlugins.mkTmuxPlugin {
  pluginName = "tmux-open-emacs";
  version = "1.0.0";

  src = ./.;

  rtpFilePath = "tmux-open-emacs.tmux";

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ tmux ];

  postInstall = ''
    # Install the CLI script
    mkdir -p $out/bin
    cp scripts/toe $out/bin/toe
    chmod +x $out/bin/toe

    # Wrap the CLI script to include necessary tools in PATH
    wrapProgram $out/bin/toe \
      --prefix PATH : ${lib.makeBinPath [ tmux ]} \
      ${lib.optionalString (
        xemacsclient != null
      ) "--set EMACSCLIENT_PATH ${xemacsclient}/bin/xemacsclient"}
  '';

  meta = with lib; {
    description = "A tmux plugin for opening files in emacsclient in the current pane";
    homepage = "https://github.com/gfanton/tmux-open-emacs";
    license = licenses.gpl3Only;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
