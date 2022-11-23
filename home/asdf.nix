{ pkgs, ... }:

{
  programs.myasdf = {
    enable = true;
    package = pkgs.asdf-vm;
    pluginsRepository = pkgs.asdf-plugins;
    tools = with pkgs; [
      # nixpkgs language managed tools
      {
        name = "golang";
        version = "system";
      }
      {
        name = "python";
        version = "system";
      }
      {
        name = "ruby";
        version = "system";
      }
      {
        name = "nodejs";
        version = "system";
      }

      # others
      {
        name = "java";
        version = "latest";
        inputs = [ unzip jq ];
      }
      {
        name = "xcodegen";
        version = "latest";
        inputs = [ unzip jq ];
      }
      {
        name = "golangci-lint";
        version = "latest";
      }

      # plugins
      { name = "yarn"; }
      {
        name = "jq";
        version = "1.6";
      }

    ];
  };
}
