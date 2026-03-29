{ pkgs }:
let
  myScripts = (import ./my-scripts.nix) { inherit pkgs; };
  getApiKey = myScripts.outsByName.emacs-get-gptel-api-key;
  git = ((import ./git.nix) {inherit pkgs; });
in
{
  outs = [
    (pkgs.writeShellApplication {
      name = "aider";
      runtimeInputs = [];
      text = ''
        # we want "/run git diff" without pager
        export GIT_PAGER=cat
        export GIT_CONFIG_GLOBAL=${git.gitconfig}

        MOONSHOT_API_BASE="https://api.moonshot.cn/v1/"
        MOONSHOT_API_KEY="$(${getApiKey} api.moonshot.cn)"
        if [ -n "$INSIDE_MSH_TEAM" ]; then
            MOONSHOT_API_BASE="https://api.msh.team/v1/"
            MOONSHOT_API_KEY="$(${getApiKey} api.msh.team)"
        fi
        OPENROUTER_API_KEY="$(${getApiKey} openrouter.ai)"

        export MOONSHOT_API_BASE MOONSHOT_API_KEY OPENROUTER_API_KEY
        exec ${pkgs.aider-chat}/bin/aider \
          --config=${./aider}/aider.conf.yml \
          --model-metadata-file ${./aider}/model.metadata.json \
          --model-settings-file ${./aider}/model.settings.yml \
          "$@"
      '';
    })
  ];
}
