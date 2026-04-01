{
  description = "Aider wrapper";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cf87881886182975bef15495007d6580c4aa6450";
    git = {
      url = "path:../git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-scripts = {
      url = "path:../my-scripts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, git, my-scripts }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          git-wrapped = git.packages.${system}.default;
          myScripts = my-scripts.packages.${system}.default;
        in
        {
          default = pkgs.writeShellApplication {
            name = "aider";
            runtimeInputs = [ myScripts.outsByName.emacs-get-gptel-api-key ];
            text = ''
              # we want "/run git diff" without pager
              export GIT_PAGER=cat
              export GIT_CONFIG_GLOBAL=${git-wrapped.gitconfig}

              MOONSHOT_API_BASE="https://api.moonshot.cn/v1/"
              MOONSHOT_API_KEY="$(emacs-get-gptel-api-key api.moonshot.cn)"
              if [ -n "$INSIDE_MSH_TEAM" ]; then
                  MOONSHOT_API_BASE="https://api.msh.team/v1/"
                  MOONSHOT_API_KEY="$(emacs-get-gptel-api-key api.msh.team)"
              fi
              OPENROUTER_API_KEY="$(emacs-get-gptel-api-key openrouter.ai)"

              export MOONSHOT_API_BASE MOONSHOT_API_KEY OPENROUTER_API_KEY
              exec ${pkgs.aider-chat}/bin/aider \
                --config=${./aider.conf.yml} \
                --model-metadata-file ${./model.metadata.json} \
                --model-settings-file ${./model.settings.yml} \
                "$@"
            '';
          };
        }
      );
    };
}
