{ pkgs }:
let
  jjconfig = pkgs.replaceVars ./jj/config.toml {
    jjPushMr = "${./jj/jj-push-mr}";
    jjMagitDiffEditor = "${./jj/jj-magit-diff-editor}";
  };
  git = ((import ./git.nix) {inherit pkgs; });

  jj = pkgs.rustPlatform.buildRustPackage rec {
    pname = "jujutsu";
    version = "0.39.0-master-lfs";
    src = pkgs.fetchFromGitHub {
      owner = "blahgeek";
      repo = "jujutsu";
      rev = "5e3a5acfe635c4ff6bbaba3fdb9d392ad586b5c9";  # 0.39.0-master-lfs
      hash = "sha256-28+wlcA913fxJH3jtAXFh1oZm0uNTfJbbGPNSFrl9No=";
    };
    cargoHash = "sha256-R1ekt62wjM59qA2z22/2ljLmNAMHTzb1Ka1BG6ui3oc=";

    doCheck = false;
    cargoBuildFlags = [
      # Don’t install the `gen-protos` build tool.
      "--bin"
      "jj"
    ];
  };
in
{
  outs = [
    (pkgs.writeShellApplication {
      name = "jj";
      runtimeInputs = [ jj pkgs.git pkgs.fzf ];
      text = ''
      export GIT_CONFIG_GLOBAL=${git.gitconfig}
      export JJ_CONFIG=${jjconfig}:~/.config/jj/config.toml
      exec jj "$@"
    '';
    })
  ];
}
