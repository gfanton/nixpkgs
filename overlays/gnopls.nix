final: super:
let inherit (super) pkgs lib;
in {
  my-gnopls = pkgs.buildGo121Module rec {
    pname = "gnopls";
    version = "0.0.3-preview";
    vendorSha256 = "sha256-rUlvOTPRwGYIUPmHozEMcmGzvak5Jex9/ACvy4vCwVg=";
    src = pkgs.fetchurl {
      url =
        "https://github.com/harry-hov/gnopls/archive/refs/tags/v${version}.tar.gz";
      sha256 = "sha256-ce+FyuF1w+xxD1JnkbCoTcSJeUbSrqieAoBpMqbSZCc=";
    };

    meta = with lib; {
      description = "";
      maintainers = [ maintainers.gfanton ];
    };
  };
}
