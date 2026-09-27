{ lib, ... }:

let
  inherit (lib) mkOption types;

in
{
  options.users.primaryUser = {
    username = mkOption {
      type = with types; nullOr str;
      default = null;
    };
    fullName = mkOption {
      type = with types; nullOr str;
      default = null;
    };
    email = mkOption {
      type = with types; nullOr str;
      default = null;
    };
    nixConfigDirectory = mkOption {
      type = with types; nullOr str;
      default = null;
    };
    sshAgent = mkOption {
      type = types.enum [
        "1password"
        "forwarded"
      ];
      default = "1password";
      description = ''
        Where the user's SSH agent comes from on this host: the local
        1Password app, or a socket another machine forwards in through
        forwardAgentTo.
      '';
    };
    sshKey = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Public key of this host's own SSH key, held by the SSH agent. ssh
        offers only this key to GitHub and to the hosts in forwardAgentTo.
      '';
    };
    forwardAgentTo = mkOption {
      type = with types; listOf str;
      default = [ ];
      description = ''
        Hosts this machine keeps its 1Password SSH agent forwarded to, each
        over a standing connection. Every host listed sets sshAgent to
        "forwarded".
      '';
    };
  };
}
