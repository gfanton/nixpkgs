{
  lib,
  stdenv,
  swift,
  rcodesign,
  runtimeShell,
}:

stdenv.mkDerivation {
  pname = "gnotify";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    swift
    rcodesign
  ];

  buildPhase = ''
    runHook preBuild
    swiftc -O gnotify.swift -o gnotify \
      -framework AppKit -framework UserNotifications
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    app=$out/Applications/gnotify.app
    mkdir -p "$app/Contents/MacOS" "$out/bin"
    cp gnotify "$app/Contents/MacOS/gnotify"

    cat >"$app/Contents/Info.plist" <<'PLIST'
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleExecutable</key><string>gnotify</string>
      <key>CFBundleIdentifier</key><string>dev.gfanton.gnotify</string>
      <key>CFBundleName</key><string>gnotify</string>
      <key>CFBundlePackageType</key><string>APPL</string>
      <key>CFBundleShortVersionString</key><string>0.1.0</string>
      <key>CFBundleVersion</key><string>0.1.0</string>
      <key>LSMinimumSystemVersion</key><string>11.0</string>
      <key>LSUIElement</key><true/>
    </dict>
    </plist>
    PLIST

    cat >"$out/bin/gnotify" <<EOF
    #!${runtimeShell}
    exec /usr/bin/open -n "$app" --args "\$@"
    EOF
    chmod +x "$out/bin/gnotify"

    runHook postInstall
  '';

  # The darwin stdenv re-signs Mach-O binaries with sigtool during fixupPhase,
  # which would strip the bundle seal. Sign last so the sealed signature wins.
  # UNUserNotificationCenter rejects an app without sealed resources; rcodesign
  # seals the bundle (Info.plist + binary) offline. Identity is the bundle id, so
  # the notification-permission grant survives rebuilds.
  postFixup = ''
    rcodesign sign "$out/Applications/gnotify.app"
  '';

  meta = {
    description = "Minimal native macOS notifier with click-to-activate and click-to-exec";
    homepage = "https://github.com/gfanton/nixpkgs";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.gfanton ];
    platforms = lib.platforms.darwin;
    mainProgram = "gnotify";
  };
}
