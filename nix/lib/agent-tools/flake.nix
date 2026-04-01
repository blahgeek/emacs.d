{
  description = "Agent tools wrappers";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cf87881886182975bef15495007d6580c4aa6450";
    my-scripts = {
      url = "path:../my-scripts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kimi-cli = {
      url = "github:MoonshotAI/kimi-cli/1.24.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, my-scripts, kimi-cli }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          myScripts = my-scripts.packages.${system}.default;
          helperDir = ./.;
          mkAgentTool = name: pkg: pkgs.writeShellApplication {
            name = name;
            runtimeInputs = [ myScripts.outsByName.emacs-get-gptel-api-key ];
            text = ''
              ${helperDir}/sandbox-run ${helperDir}/${name}.bash ${pkg}/bin/${name} "$@"
            '';
          };
        in
        {
          default = pkgs.symlinkJoin {
            name = "agent-tools";
            paths = [
              (mkAgentTool "claude" pkgs.claude-code)
              (mkAgentTool "codex" pkgs.codex)
              (mkAgentTool "gemini" pkgs.gemini-cli)
              (mkAgentTool "kimi" kimi-cli.packages.${system}.default)
            ];
          };
        }
      );
    };
}
