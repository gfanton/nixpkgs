final: super:
let
  inherit (super) pkgs lib;
in
{
  my-rtk = pkgs.rustPlatform.buildRustPackage rec {
    pname = "rtk";
    version = "0.50.0";

    src = pkgs.fetchFromGitHub {
      owner = "rtk-ai";
      repo = "rtk";
      rev = "v${version}";
      hash = "sha256-cQq+iJ6L7YTc9oinNw1X+qt8PkDhYM/mi7tXMJf7fp8=";
    };

    cargoHash = "sha256-COpR8TZJgim/WxRG//bEKc4tAEWy0GfkGcFK/dnpRlQ=";

    nativeBuildInputs = [ pkgs.makeWrapper ];

    doCheck = false;

    postInstall = ''
      install -Dm755 $src/hooks/claude/rtk-rewrite.sh $out/libexec/rtk/hooks/rtk-rewrite.sh
      wrapProgram $out/libexec/rtk/hooks/rtk-rewrite.sh \
        --prefix PATH : ${lib.makeBinPath [ pkgs.jq ]}:$out/bin

      install -Dm644 $src/hooks/rtk-awareness-high.md $out/share/rtk/RTK.md
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
