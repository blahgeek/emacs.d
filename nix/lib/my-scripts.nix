{ pkgs }:
rec {
  outsByName =
    builtins.mapAttrs (name: _: pkgs.writeShellApplication {
      name = name;
      runtimeInputs = [];
      text = builtins.readFile ./my-scripts/${name};
      checkPhase = "";
    })
      (pkgs.lib.filterAttrs (name: value: value == "regular")
        (builtins.readDir ./my-scripts));

  outs = pkgs.lib.mapAttrsToList (_: v: v) outsByName;
}
