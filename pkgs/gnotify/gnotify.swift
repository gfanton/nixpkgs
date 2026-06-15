import AppKit
import UserNotifications

// gnotify — a minimal macOS notifier built on UNUserNotificationCenter.
//
// Posts a Notification Center alert and, when the banner is clicked, activates a
// target app by bundle id and/or runs a shell command. The command carries the
// tmux jump (switch-client/select-pane) so a click lands on the originating pane.
//
// Must run as a signed .app bundle: UNUserNotificationCenter refuses to post from
// a bare CLI binary. The $out/bin/gnotify wrapper launches the bundle via `open`.

// ---- Options

struct Options {
    var title = ""
    var message = ""
    var sound: String?
    var activateBundleID: String?
    var execCommand: String?
    var timeout: Double = 30
}

func parseArgs(_ args: [String]) -> Options {
    var opts = Options()
    var i = 0
    func value() -> String? {
        i += 1
        return i < args.count ? args[i] : nil
    }
    while i < args.count {
        switch args[i] {
        case "--title": opts.title = value() ?? ""
        case "--message", "--body": opts.message = value() ?? ""
        case "--sound": opts.sound = value()
        case "--activate": opts.activateBundleID = value()
        case "--exec": opts.execCommand = value()
        case "--timeout": if let v = value(), let d = Double(v) { opts.timeout = d }
        default: break // ignore unknown args (e.g. LaunchServices' -psn_…)
        }
        i += 1
    }
    return opts
}

func warn(_ message: String) {
    FileHandle.standardError.write(Data("gnotify: \(message)\n".utf8))
}

// ---- Delegate

final class Delegate: NSObject, NSApplicationDelegate, UNUserNotificationCenterDelegate {
    let opts: Options

    init(opts: Options) { self.opts = opts }

    func applicationDidFinishLaunching(_ note: Notification) {
        let center = UNUserNotificationCenter.current()
        center.delegate = self
        center.requestAuthorization(options: [.alert, .sound]) { granted, error in
            DispatchQueue.main.async {
                if let error { warn("authorization error: \(error.localizedDescription)") }
                guard granted else {
                    warn("notifications not authorized")
                    NSApp.terminate(nil)
                    return
                }
                self.post(to: center)
            }
        }
    }

    func post(to center: UNUserNotificationCenter) {
        let content = UNMutableNotificationContent()
        content.title = opts.title
        content.body = opts.message
        if let sound = opts.sound {
            content.sound = sound.lowercased() == "default"
                ? .default
                : UNNotificationSound(named: UNNotificationSoundName(sound))
        }
        var info: [String: String] = [:]
        if let id = opts.activateBundleID { info["activate"] = id }
        if let cmd = opts.execCommand { info["exec"] = cmd }
        content.userInfo = info

        let request = UNNotificationRequest(
            identifier: UUID().uuidString, content: content, trigger: nil)
        center.add(request) { error in
            if let error { warn("post error: \(error.localizedDescription)") }
        }

        // No click within the window: exit so we don't linger as an agent process.
        DispatchQueue.main.asyncAfter(deadline: .now() + opts.timeout) {
            NSApp.terminate(nil)
        }
    }

    // Present even though we're the active (accessory) app posting the notification.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        willPresent notification: UNNotification,
        withCompletionHandler completionHandler: @escaping (UNNotificationPresentationOptions) -> Void
    ) {
        completionHandler([.banner, .sound, .list])
    }

    // Banner click: activate the target app, run the embedded command, exit.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse,
        withCompletionHandler completionHandler: @escaping () -> Void
    ) {
        if response.actionIdentifier == UNNotificationDefaultActionIdentifier {
            let info = response.notification.request.content.userInfo
            if let id = info["activate"] as? String {
                for app in NSRunningApplication.runningApplications(withBundleIdentifier: id) {
                    app.activate(options: [.activateIgnoringOtherApps])
                }
            }
            if let cmd = info["exec"] as? String, !cmd.isEmpty {
                let process = Process()
                process.executableURL = URL(fileURLWithPath: "/bin/sh")
                process.arguments = ["-c", cmd]
                do {
                    try process.run()
                    process.waitUntilExit()
                } catch {
                    warn("exec failed: \(error.localizedDescription)")
                }
            }
        }
        completionHandler()
        NSApp.terminate(nil)
    }
}

// ---- Entry point

let opts = parseArgs(Array(CommandLine.arguments.dropFirst()))
let app = NSApplication.shared
let delegate = Delegate(opts: opts)
app.delegate = delegate
app.setActivationPolicy(.accessory) // agent app: no Dock icon, no menu bar
app.run()
