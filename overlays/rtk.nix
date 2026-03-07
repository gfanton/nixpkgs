final: super:
let
  inherit (super) pkgs lib;
in
{
  my-rtk = pkgs.rustPlatform.buildRustPackage rec {
    pname = "rtk";
    version = "0.25.0";

    src = pkgs.fetchFromGitHub {
      owner = "rtk-ai";
      repo = "rtk";
      rev = "v${version}";
      hash = "sha256-pM57pJmygJ2yAIQjhraKuFORj9stLDw040o1TIu5a+g=";
    };

    cargoHash = "sha256-F4mT1QPHV17ehghBXY8YDBMp+LfAIVNoTlhq4aZnWmU=";

    nativeBuildInputs = [ pkgs.makeWrapper ];

    doCheck = false;

    postInstall = ''
      install -Dm755 $src/hooks/rtk-rewrite.sh $out/libexec/rtk/hooks/rtk-rewrite.sh
      wrapProgram $out/libexec/rtk/hooks/rtk-rewrite.sh \
        --prefix PATH : ${lib.makeBinPath [ pkgs.jq ]}:$out/bin
    '';

    meta = with lib; {
      description = "CLI proxy that reduces LLM token consumption by 60-90%";
      homepage = "https://github.com/rtk-ai/rtk";
      license = licenses.mit;
      mainProgram = "rtk";
      platforms = platforms.unix;
    };
  };
}
