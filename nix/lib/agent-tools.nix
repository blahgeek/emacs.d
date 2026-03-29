{ pkgs }:
let
  helperDir = ./agent-tools ;
  myScripts = (import ./my-scripts.nix) { inherit pkgs; };

  mkAgentTool = (name: pkg: pkgs.writeShellApplication {
    name = name;
    runtimeInputs = [ myScripts.outsByName.emacs-get-gptel-api-key ];
    text = ''
      ${helperDir}/sandbox-run ${helperDir}/${name}.bash ${pkg}/bin/${name} "$@"
    '';
  });
in
{
  outs = [
    (mkAgentTool "claude" pkgs.claude-code)
    (mkAgentTool "codex" pkgs.codex)
    (mkAgentTool "gemini" pkgs.gemini-cli)
    (mkAgentTool "kimi" ((import ./flake-compat.nix) {
      src = fetchTarball {
        url = "https://github.com/MoonshotAI/kimi-cli/archive/refs/tags/1.24.0.tar.gz";
        sha256 = "1ds0w6m1nv2l84g0hd78ibgsy7sx0ys9qyrdcd1dgc3bzn6k5rm2";
      };
    }).defaultNix.packages.${pkgs.stdenv.hostPlatform.system}.default)
  ];
}
