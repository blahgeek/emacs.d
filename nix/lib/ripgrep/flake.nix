{
  description = "ripgrep with custom config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/cf87881886182975bef15495007d6580c4aa6450";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          default = pkgs.symlinkJoin {
            name = "ripgrep-wrapped";
            paths = [ pkgs.ripgrep ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/rg \
                --set RIPGREP_CONFIG_PATH ${./ripgrep.config}
            '';
          };
        }
      );
    };
}
