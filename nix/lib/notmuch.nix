{ pkgs }:
let
  notmuchConfig = pkgs.replaceVars ./notmuch/config {
    hookDir = "${./notmuch/hooks}";
  };
in
{
  outs = [
    (pkgs.symlinkJoin {
      name = "notmuch-wrapped";
      paths = [ pkgs.notmuch ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/notmuch \
          --set NOTMUCH_CONFIG ${notmuchConfig}
      '';
    })
    pkgs.notmuch.emacs
  ];
}
