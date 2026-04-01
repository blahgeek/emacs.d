{...}:

let
  pkgs = origPkgs // myPkgs;
  origPkgs = import (fetchTarball {
    # 2026.3.22
    url = "https://github.com/NixOS/nixpkgs/archive/cf87881886182975bef15495007d6580c4aa6450.tar.gz";
    sha256 = "0mxr1b4g6wfbdsyz029ibc6pah43ci0cjl3k13y4i8k3z9sra9by";
  }) {
    config.doCheckByDefault = false;
    config.allowUnfree = true;
    overlays = [
      (import (builtins.fetchTarball {
        # 2026.3.25
        url = "https://github.com/nix-community/emacs-overlay/archive/35e79fe95d7cec6365a08e3759819420e89b73f2.tar.gz";
        sha256 = "0n89ivl7sh2qnqn4id3xk33sc03zm67i2sv6w9pb4p93kpj5zjg5";
      }))
    ];
  };

  myPkgs = {

    jujutsu = pkgs.rustPlatform.buildRustPackage rec {
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

    kimi-cli = (flake-compat {
      src = fetchTarball {
        url = "https://github.com/MoonshotAI/kimi-cli/archive/refs/tags/1.24.0.tar.gz";
        sha256 = "1ds0w6m1nv2l84g0hd78ibgsy7sx0ys9qyrdcd1dgc3bzn6k5rm2";
      };
    }).defaultNix.packages.${pkgs.stdenv.hostPlatform.system}.default;

    xonsh = (
      let python312 = pkgs.python312.override {
            packageOverrides = final: prev: rec {
              xonsh = prev.xonsh.overridePythonAttrs (prev: rec {
                # https://github.com/xonsh/xonsh/pull/6026
                version = "0.22.0-fix-completer";
                src = pkgs.fetchFromGitHub {
                  owner = "blahgeek";
                  repo = "xonsh";
                  rev = "1e239faed7a16e1b098acb503f1d884e719e8607";
                  hash = "sha256-EQiK1d60F/rHX1K+S6KqpzL82ssulECeasSiAkQ+Ah0=";
                };
                doCheck = false;
              });
            };
          };
      in
        (origPkgs.xonsh.override {
          python3 = python312;
          extraPackages = ps: [
            ps.xonsh.xontribs.xontrib-abbrevs
            (with ps; buildPythonPackage {
              pname = "xontrib-autojump";
              version = "1.4";
              src = pkgs.fetchFromGitHub {
                owner = "wshanks";
                repo = "xontrib-autojump";
                tag = "v1.4";
                hash = "sha256-IhF40olhMR5Ymu57kDu8jzD4QCjd6wMzHcsubNExpaA=";
              };
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
      version = "5f702a26";
      src = pkgs.fetchFromGitHub {
        owner = "blahgeek";
        repo = "emacs-lsp-booster";
        rev = "5f702a2699f306a3958ff1996a2b1a625f0cee0b";
        hash = "sha256-R9v+hCma/FfYdR+fvZ0vmtVk4dm+bPBacwV1QCc6X+8=";
      };
      cargoHash = "sha256-qchwxW3KITQcv6EFzR2BSISWB2aTW9EdCN/bx5m0l48=";
      doCheck = false;
    });
  };

  flake-compat = import (fetchTarball {
    # 2026.01.24
    url = "https://github.com/edolstra/flake-compat/archive/5edf11c44bc78a0d334f6334cdaf7d60d732daab.tar.gz";
    sha256 = "0yqfa6rx8md81bcn4szfp0hjq2f3h9i8zjzhqqyfqdkrj5559nmw";
  });

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
    runtimeInputs = [ myScripts.emacs-get-gptel-api-key ];
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
    text = builtins.readFile ./etc/my-scripts/${name};
    checkPhase = "";
  }) (pkgs.lib.filterAttrs (name: value: value == "regular") (builtins.readDir ./etc/my-scripts));

in

pkgs.buildEnv {
  name = "home";
  pathsToLink = ["/bin" "/etc" "/include" "/lib" "/libexec" "/sbin" "/share"];
  paths = [

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
