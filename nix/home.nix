{...}:

let
  sources = import ./sources.nix;

  pkgs = origPkgs // myPkgs;
  origPkgs = import sources.nixpkgs {
    config.doCheckByDefault = false;
    config.allowUnfree = true;
    overlays = [
      (import sources.emacs-overlay)
    ];
  };

  myPkgs = {

    jujutsu = pkgs.rustPlatform.buildRustPackage rec {
      pname = "jujutsu";
      version = sources.jujutsu.rev;
      src = sources.jujutsu;
      cargoHash = "sha256-R1ekt62wjM59qA2z22/2ljLmNAMHTzb1Ka1BG6ui3oc=";

      doCheck = false;
      cargoBuildFlags = [
        # Don’t install the `gen-protos` build tool.
        "--bin"
        "jj"
      ];
    };

    kimi-cli = (flake-compat {
      src = sources.kimi-cli;
    }).defaultNix.packages.${pkgs.stdenv.hostPlatform.system}.default;

    xonsh = (
      (origPkgs.xonsh.override {
        extraPackages = ps: [
          ps.xonsh.xontribs.xontrib-abbrevs
          (with ps; buildPythonPackage {
            pname = "xontrib-autojump";
            version = sources.xontrib-autojump.rev;
            src = sources.xontrib-autojump;
            pyproject = true;
            build-system = [
              setuptools
            ];
          })
        ];
      })
    );

    emacs-lsp-booster = (pkgs.rustPlatform.buildRustPackage rec {
      pname = "emacs-lsp-booster";
      version = sources.emacs-lsp-booster.rev;
      src = sources.emacs-lsp-booster;
      cargoHash = "sha256-qchwxW3KITQcv6EFzR2BSISWB2aTW9EdCN/bx5m0l48=";
      doCheck = false;
    });
  };

  flake-compat = import sources.flake-compat;

  mkWrapperWithEnv = name: pkg: envs : (pkgs.symlinkJoin {
    name = "${name}-wrapped";
    paths = [ pkg ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = "wrapProgram $out/bin/${name} " +
                (pkgs.lib.concatStringsSep
                  " "
                  (pkgs.lib.mapAttrsToList (k: v: "--set ${k} ${v}") envs));
  });
  mkAgentTool = (name: pkg: pkgs.writeShellApplication {
    name = name;
    runtimeInputs = [ myScripts.emacs-get-gptel-api-key myScripts.emacsclient-on-current-server ];
    bashOptions = [];  # "errexit" "nounset" "pipefail"
    text = ''
      ${./etc/agent-tools}/sandbox-run ${./etc/agent-tools}/${name}.bash ${pkg}/bin/${name} "$@"
    '';
  });


  gitconfig = pkgs.replaceVars ./etc/git/config {
    gitignore = "${./etc/git/ignore}";
  };
  jjconfig = pkgs.replaceVars ./etc/jj/config.toml {
    jjPushMr = "${./etc/jj/jj-push-mr}";
    jjMagitDiffEditor = "${./etc/jj/jj-magit-diff-editor}";
  };

  myScripts = builtins.mapAttrs (name: _: pkgs.writeShellApplication {
    name = name;
    runtimeInputs = [];
    bashOptions = [];  # "errexit" "nounset" "pipefail"
    text = builtins.readFile ./etc/my-scripts/${name};
    checkPhase = "";
  }) (pkgs.lib.filterAttrs (name: value: value == "regular") (builtins.readDir ./etc/my-scripts));

in

{
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;

  home.packages = [

    (pkgs.emacs-git-nox.override {
      withNativeCompilation = true;
      withSelinux = false;
      withSystemd = false;
      withCompressInstall = false;
    })

    (mkAgentTool "claude" pkgs.claude-code)
    (mkAgentTool "codex" pkgs.codex)
    (mkAgentTool "gemini" pkgs.gemini-cli)
    (mkAgentTool "kimi" pkgs.kimi-cli)
    (
      pkgs.writeShellApplication {
        name = "aider";
        runtimeInputs = [ myScripts.emacs-get-gptel-api-key pkgs.aider-chat ];
        text = ''
        export GIT_CONFIG_GLOBAL=${gitconfig}
        # shellcheck source=/dev/null
        source ${./etc/aider}/aider-wrapper.bash
        '';
      }
    )

    (mkWrapperWithEnv "git" pkgs.git {
      GIT_CONFIG_GLOBAL = gitconfig;
    })
    (mkWrapperWithEnv "rg" pkgs.ripgrep {
      RIPGREP_CONFIG_PATH = ./etc/ripgrep/ripgrep.config;
    })
    (pkgs.writeShellApplication {
      name = "jj";
      runtimeInputs = [ pkgs.jujutsu pkgs.git pkgs.fzf ];
      text = ''
        export GIT_CONFIG_GLOBAL=${gitconfig}
        export JJ_CONFIG=${jjconfig}:~/.config/jj/config.toml
        exec jj "$@"
      '';
    })

    (mkWrapperWithEnv "notmuch" pkgs.notmuch {
      NOTMUCH_CONFIG = pkgs.replaceVars ./etc/notmuch/config {
        hookDir = "${./etc/notmuch/hooks}";
      };
    })
    pkgs.notmuch.emacs

    (pkgs.librime.override {
      plugins = [
        (pkgs.librime-lua.override {
          # https://github.com/iDvel/rime-ice/issues/840
          lua = pkgs.lua5_4;
        })
      ];
    })
    pkgs.rime-ice

    pkgs.ast-grep
    pkgs.autojump
    pkgs.bazel-buildtools  # buildifier
    pkgs.bazelisk
    pkgs.bind.dnsutils
    pkgs.bitwarden-cli
    pkgs.bubblewrap
    pkgs.clang-tools
    pkgs.clickhouse
    pkgs.cpplint
    (pkgs.curl.override { c-aresSupport = !pkgs.stdenv.isDarwin; })
    pkgs.docker-client
    pkgs.docker-compose
    pkgs.dtrx
    pkgs.emacs-lsp-booster
    pkgs.fd
    pkgs.file
    pkgs.ffmpeg
    pkgs.flamegraph
    pkgs.fzf
    pkgs.gawk  # install as awk directly
    pkgs.git-lfs
    pkgs.glab
    pkgs.gnupg
    pkgs.go
    pkgs.go-jsonnet
    pkgs.golangci-lint
    pkgs.google-cloud-sdk
    pkgs.gopls
    pkgs.hurl
    pkgs.htop
    pkgs.ipatool
    pkgs.iperf
    pkgs.just
    pkgs.jq
    pkgs.kubectl
    pkgs.less
    pkgs.moreutils
    pkgs.mtr
    pkgs.ncdu
    pkgs.neovim
    pkgs.niv
    pkgs.nmap
    pkgs.nodejs
    pkgs.pre-commit
    pkgs.pv
    pkgs.pwgen
    pkgs.pyright
    pkgs.python312Packages.httpie
    pkgs.python312Packages.markdown2
    # yes, do not enable mount in linux.
    # when enabled, it would use "fusermount3" in nix, which does not have setuid bit set.
    # when disabled, the feature is still present, but it would use system's fusermount3
    (pkgs.rclone.override { enableCmount = pkgs.stdenv.isDarwin; })
    pkgs.rsync
    pkgs.rustup
    pkgs.socat
    pkgs.strace
    pkgs.time
    pkgs.tmux
    pkgs.typos-lsp
    pkgs.unrar
    pkgs.uv
    pkgs.w3m-nox
    pkgs.whois
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
    pkgs.xonsh
  ]
  ++ pkgs.lib.mapAttrsToList (_: v: v) myScripts;
}
