{
  description = "Notmuch with custom config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/cf87881886182975bef15495007d6580c4aa6450";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          notmuchConfig = pkgs.replaceVars ./config {
            hookDir = "${./hooks}";
          };
        in
        {
          default = pkgs.symlinkJoin {
            name = "notmuch-wrapped";
            paths = [ pkgs.notmuch ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/notmuch \
                --set NOTMUCH_CONFIG ${notmuchConfig}
            '';
          } // { inherit (pkgs.notmuch) emacs; };
        }
      );
    };
}
