final: prev:
let
  # Helper to extract a sub-plugin from the official marketplace repo
  mkOfficialPlugin =
    name:
    prev.stdenvNoCC.mkDerivation {
      pname = "claude-plugin-${name}";
      version = "unstable";
      src = final.claude-plugins-official-src;
      dontBuild = true;
      installPhase = ''
        cp -r plugins/${name} $out
      '';
    };
in
{
  claude-plugins = {
    # Standalone plugin (whole repo is the plugin)
    superpowers = final.claude-plugin-superpowers;

    # Sub-plugins extracted from anthropics/claude-plugins-official
    frontend-design = mkOfficialPlugin "frontend-design";
    skill-creator = mkOfficialPlugin "skill-creator";

    # Matt Pocock's skills — single plugin (the whole repo is the plugin,
    # `.claude-plugin/plugin.json` at root enumerates all skills by path)
    pocock-skills = prev.stdenvNoCC.mkDerivation {
      pname = "claude-plugin-pocock-skills";
      version = "unstable";
      src = final.pocock-skills-src;
      dontBuild = true;
      installPhase = "cp -r . $out";
    };
  };
}
