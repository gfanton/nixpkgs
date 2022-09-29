final: super:
let pkgs = super.pkgs;
in {
  my-libvterm = pkgs.stdenv.mkDerivation rec {
    pname = "libvterm";
    version = "0.3-RC1";

    src = pkgs.fetchurl {
      url =
        "https://launchpad.net/libvterm/trunk/v0.3/+download/libvterm-${version}.tar.gz";
      sha256 =
        "441d1c372b84a0df12525100ab06c0366260fb4f6252abd1665ee4fa571b5134";
    };

    nativeBuildInputs = with pkgs; [ libtool gnulib pkg-config ];

    preInstall = ''
      mkdir -p $out/include
      mkdir -p $out/lib
    '';

    preBuild = ''
      makeFlagsArray+=(PREFIX=$out LIBTOOL="libtool")
    '';

    buildInputs = with pkgs; [ gcc perl glib ncurses ];

    meta = with pkgs.lib; {
      homepage = "https://launchpad.net/libvterm";
      description =
        "An abstract library implementation of a DEC VT/xterm/ECMA-48 terminal emulator.";
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };
}
