{ pkgs }:
{
  outs = [
    (pkgs.symlinkJoin {
      name = "ripgrep-wrapped";
      paths = [ pkgs.ripgrep ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/rg \
          --set RIPGREP_CONFIG_PATH ${./ripgrep/ripgrep.config}
      '';
    })
  ];
}
