{
  description = "Git with custom global config";

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
          gitconfig = pkgs.replaceVars ./config {
            gitignore = "${./ignore}";
          };
        in
        {
          default = pkgs.symlinkJoin {
            name = "git-wrapped";
            paths = [ pkgs.git ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/git \
                --set GIT_CONFIG_GLOBAL ${gitconfig}
            '';
          } // { inherit gitconfig; };
        }
      );
    };
}
