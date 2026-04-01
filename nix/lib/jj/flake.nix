{
  description = "Jujutsu with custom config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cf87881886182975bef15495007d6580c4aa6450";
    git.url = "path:../git";
    git.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, git }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          git-wrapped = git.packages.${system}.default;
          jjconfig = pkgs.replaceVars ./config.toml {
            jjPushMr = "${./jj-push-mr}";
            jjMagitDiffEditor = "${./jj-magit-diff-editor}";
          };
          jj = pkgs.rustPlatform.buildRustPackage rec {
            pname = "jujutsu";
            version = "0.39.0-master-lfs";
            src = pkgs.fetchFromGitHub {
              owner = "blahgeek";
              repo = "jujutsu";
              rev = "5e3a5acfe635c4ff6bbaba3fdb9d392ad586b5c9";
              hash = "sha256-28+wlcA913fxJH3jtAXFh1oZm0uNTfJbbGPNSFrl9No=";
            };
            cargoHash = "sha256-R1ekt62wjM59qA2z22/2ljLmNAMHTzb1Ka1BG6ui3oc=";
            doCheck = false;
            cargoBuildFlags = [ "--bin" "jj" ];
          };
        in
        {
          default = pkgs.writeShellApplication {
            name = "jj";
            runtimeInputs = [ jj pkgs.git pkgs.fzf ];
            text = ''
              export GIT_CONFIG_GLOBAL=${git-wrapped.gitconfig}
              export JJ_CONFIG=${jjconfig}:~/.config/jj/config.toml
              exec jj "$@"
            '';
          };
        }
      );
    };
}
