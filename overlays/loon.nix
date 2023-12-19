final: super:
let inherit (super) pkgs lib;
in {
  my-loon = pkgs.buildGo121Module rec {
    pname = "loon";
    version = "1.5.0";
    vendorHash = "sha256-GyBD1Wl7HFP1jwjUPh7mC8e6SS2ppSpAyZvo4XRjn/U=";
    src = pkgs.fetchurl {
      url =
        "https://github.com/gfanton/loon/archive/refs/tags/v${version}.tar.gz";
      sha256 = "sha256-9H1YKVXrTqoadMBNkn05UQSosveVlDsTLWCAIZ98Z/M=";
    };

    meta = with lib; {
      description = "dynamic realtime pager";
      maintainers = [ maintainers.gfanton ];
    };
  };
}
