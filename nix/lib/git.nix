{ pkgs }:
let
  gitconfig = pkgs.replaceVars ./git/config {
    gitignore = "${./git/ignore}";
  };
in
{
  inherit gitconfig;
  outs = [
    (pkgs.symlinkJoin {
      name = "git-wrapped";
      paths = [ pkgs.git ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/git \
          --set GIT_CONFIG_GLOBAL ${gitconfig}
      '';
    })
  ];
}
