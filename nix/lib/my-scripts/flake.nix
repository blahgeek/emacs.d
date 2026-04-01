{
  description = "My custom shell scripts";

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
          scriptFiles = pkgs.lib.filterAttrs (name: value: value == "regular" && !pkgs.lib.hasPrefix "flake" name)
            (builtins.readDir ./.);
          outsByName = builtins.mapAttrs (name: _: pkgs.writeShellApplication {
            name = name;
            runtimeInputs = [];
            text = builtins.readFile ./${name};
            checkPhase = "";
          }) scriptFiles;
        in
        {
          default = pkgs.symlinkJoin {
            name = "my-scripts";
            paths = pkgs.lib.mapAttrsToList (_: v: v) outsByName;
          } // { inherit outsByName; };
        }
      );
    };
}
