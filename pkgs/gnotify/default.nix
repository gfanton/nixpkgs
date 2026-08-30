{
  lib,
  stdenv,
  swift,
  rcodesign,
  runtimeShell,
  imagemagick,
  libicns,
  # Image shown on the leading edge of every banner. macOS takes that icon from
  # the posting app's bundle and no public API overrides it per notification, so
  # it is chosen here rather than passed at call time. Null keeps the notifier
  # unbranded, which is the right default for a tool any caller can use; a
  # consumer that wants its own identity overrides it.
  icon ? null,
}:

stdenv.mkDerivation {
  pname = "gnotify";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    swift
    rcodesign
  ]
  # macOS ships sips and iconutil, but neither reaches the build sandbox, so the
  # icns is assembled from nixpkgs tools instead.
  ++ lib.optionals (icon != null) [
    imagemagick
    libicns
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

    ${lib.optionalString (icon != null) ''
      # icns holds a fixed ladder of square sizes and png2icns maps each source
      # to the matching rung. Every rung is supplied rather than just the
      # largest: a notification badge is drawn small, and downscaling one 512px
      # entry is left to macOS's discretion. imagemagick alone will happily
      # write a plain png under an .icns name, which macOS then ignores.
      mkdir -p "$app/Contents/Resources"
      for size in 16 32 128 256 512; do
        magick ${icon} -resize "''${size}x''${size}" "icon-''${size}.png"
      done
      png2icns "$app/Contents/Resources/AppIcon.icns" \
        icon-16.png icon-32.png icon-128.png icon-256.png icon-512.png
    ''}

    cat >"$app/Contents/Info.plist" <<'PLIST'
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
      <key>CFBundleExecutable</key><string>gnotify</string>
      <key>CFBundleIdentifier</key><string>dev.gfanton.gnotify</string>
      <key>CFBundleName</key><string>gnotify</string>${lib.optionalString (icon != null) ''

      <key>CFBundleIconFile</key><string>AppIcon</string>''}
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
