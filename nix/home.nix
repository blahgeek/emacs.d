{...}:

let
  sources = import ./sources.nix;

  pkgs = origPkgs // myPkgs;
  origPkgs = import sources.nixpkgs {
    config.doCheckByDefault = false;
    config.allowUnfree = true;
  };
  lib = origPkgs.lib;

  myPkgs = {

    emacs = (let
      repoMeta = sources.emacs-git;
    in
      (origPkgs.emacs-nox.override {
        srcRepo = true;
        withSelinux = false;
        withSystemd = false;
        withCompressInstall = false;
        withNativeCompilation = true;
      }).overrideAttrs (old: {
        name = "emacs-nox-${repoMeta.branch}-${repoMeta.rev}";
        version = repoMeta.rev;
        src = repoMeta;
        patches = [];
        dontStrip = true;
        CFLAGS = (old.CFLAGS or "") + " -g3";
        CXXFLAGS = (old.CXXFLAGS or "") + " -g3";
        postPatch = old.postPatch + ''
          substituteInPlace lisp/loadup.el \
            --replace-warn '(emacs-repository-get-version)' '"${repoMeta.rev}"' \
            --replace-warn '(emacs-repository-get-branch)' '"${repoMeta.branch}"'
        '';
      })
    );

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

    lark-cli = (pkgs.buildGoModule {
      name = "lark-cli";
      src = sources.lark-cli;
      vendorHash = "sha256-NvDwhcY/L7d+zSDmrOs50oJD9cbcbWxsw1ONr3dpwlY=";
      doCheck = false;
    }).overrideAttrs(old: {
      postInstall = (old.postInstall or "") + ''
        mv $out/bin/cli $out/bin/lark-cli
      '';
    });

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
      cargoHash = "sha256-7lIceMT2hJplHU2VIN1O8IiGE6+DxO4/uM8pYS/qvlE=";
      doCheck = false;
    });
  };

  flake-compat = import sources.flake-compat;
  pyproject-nix = (import sources.pyproject-nix) { inherit lib; };
  uv2nix = (import sources.uv2nix) { inherit lib pyproject-nix; };

  # NOTE: 为什么倾向于用wrapper把配置文件和exe绑定，而不是用home manager的文件管理拷贝到~/下？
  # 因为这样运行sandbox的时候直接就能用，不需要记得暴露~/下的各种目录
  # 然后 ~/ 下的可以作为node-local的配置去引用（如果支持的话）
  mkWrapperWithEnv = name: pkg: envs : (pkgs.symlinkJoin {
    name = "${name}-wrapped";
    paths = [ pkg ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = "wrapProgram $out/bin/${name} " +
                (pkgs.lib.concatStringsSep
                  " "
                  (pkgs.lib.mapAttrsToList (k: v: "--set ${k} ${v}") envs));
  });
  mkAgentTool = (name: pkg: envs: pkgs.writeShellApplication {
    name = name;
    runtimeInputs = [ myScripts."emacs-auth-source-get.py" myScripts.emacsclient-on-current-server ];
    bashOptions = [];  # "errexit" "nounset" "pipefail"
    text = ''
      ${pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (k: v: "export ${k}=${v}") envs)}
      export SKILLS_DIR=${agentSkills}
      exec ${./etc/agent-tools}/sandbox-run ${./etc/agent-tools}/${name}.bash ${pkg}/bin/${name} "$@"
    '';
  });

  agentSkills = pkgs.runCommand "agent-skills" {} ''
    # symlinkJoin would create links at leaf level (aka, files are links, dirs are not),
    # which would break codex skill discovery. so we need to create links using commands.
    mkdir -p $out
    # local skills from etc/agent-skills (directory-level symlinks)
    for d in ${./etc/agent-skills}/*/; do
      ln -s "$d" "$out/$(basename "$d")"
    done
    # lark-cli skills
    for skill in lark-doc lark-drive lark-contact lark-im lark-openapi-explorer lark-shared lark-whiteboard lark-wiki; do
      ln -s "${sources.lark-cli}/skills/$skill" "$out/$skill"
    done
    # I don't like agent-browser, it bundles many other skills
    # see playwright-cli below
    ln -s ${pkgs.playwright}/lib/tools/cli-client/skill $out/playwright-cli
  '';
  gitconfig = pkgs.replaceVars ./etc/git/config {
    gitignore = "${./etc/git/ignore}";
  };
  jjconfig = pkgs.replaceVars ./etc/jj/config.toml {
    jjPushMr = "${./etc/jj/jj-push-mr}";
    jjMagitDiffEditor = "${./etc/jj/jj-magit-diff-editor}";
  };

  myScripts =
    builtins.mapAttrs
      (name: _: pkgs.writeShellApplication {
        name = name;
        runtimeInputs = [ pkgs.jq pkgs.curl ];  # set some common tools
        bashOptions = [];  # "errexit" "nounset" "pipefail"
        text = builtins.readFile ./etc/my-scripts/${name};
        checkPhase = "";
      })
      (lib.filterAttrs
        (name: type: type == "regular" && !(lib.hasSuffix ".py" name) && !(lib.hasSuffix ".lock" name))
        (builtins.readDir ./etc/my-scripts))
    //
    lib.mapAttrs'
      (name: _:
        if builtins.pathExists ./etc/my-scripts/${name}.lock then
          (let
            script = uv2nix.lib.scripts.loadScript { script = ./etc/my-scripts/${name}; };
            overlay = script.mkOverlay { sourcePreference = "wheel"; };
            pythonSet = (pkgs.callPackage pyproject-nix.build.packages {
              python = pkgs.python3;
            }).overrideScope overlay;
          in {
            name = name;  # keep ".py" in name
            value = pkgs.writeScriptBin name (
              script.renderScript {
                venv = script.mkVirtualEnv { inherit pythonSet; };
              }
            );
          })
        else ({
          name = name;  # keep ".py" in name
          value = pkgs.writers.writePython3Bin name {} (builtins.readFile ./etc/my-scripts/${name});
        })
      )
      (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".py" name)
        (builtins.readDir ./etc/my-scripts))
  ;

in

{
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;

  home.packages = [

    (mkAgentTool "claude" pkgs.claude-code {})
    (mkAgentTool "codex" pkgs.codex {})
    (mkAgentTool "kimi" pkgs.kimi-cli {})
    (mkAgentTool "agent-sandbox-dummy" (pkgs.symlinkJoin {
      name = "agent-sandbox-dummy";
      paths = [ pkgs.bash ];
      postBuild = ''
        ln -sf $out/bin/bash $out/bin/agent-sandbox-dummy
      '';
    }) {})
    (mkAgentTool "pi" pkgs.pi-coding-agent {
      _MODELS_JSON = pkgs.runCommand "pi-models.json" {} ''
        ${pkgs.nodejs}/bin/node ${./etc/agent-tools}/pi/generate-models.mjs ${pkgs.pi-coding-agent} > $out
      '';
    })
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

    # https://github.com/microsoft/playwright-cli/blob/main/playwright-cli.js
    # it's a simple wrapper around playwright-core/lib/tools/cli-client/program
    # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/web/playwright/driver.nix
    # recover the "lib/node_modules" layout from "playwright" package (aka, playwright-core)
    (mkWrapperWithEnv "playwright-cli" (pkgs.writers.writeJSBin "playwright-cli" {
      libraries = [ (
        pkgs.runCommand "playwright-core" {} ''
        mkdir -p $out/lib/node_modules
        ln -s "${pkgs.playwright}" $out/lib/node_modules/playwright-core
      '') ];
    } ''
      const { program } = require('playwright-core/lib/tools/cli-client/program');
      program({});
    '') { PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright.browsers-chromium}";
          PLAYWRIGHT_MCP_BROWSER = "chromium";
          PLAYWRIGHT_MCP_OUTPUT_DIR = "/tmp/playwright-cli";
        })

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
    pkgs.emacs
    pkgs.emacs-lsp-booster
    pkgs.fd
    pkgs.ffmpeg
    pkgs.file
    pkgs.flamegraph
    pkgs.fzf
    pkgs.gawk  # install as awk directly
    pkgs.gh
    pkgs.git-lfs
    pkgs.glab
    pkgs.gnupg
    pkgs.go
    pkgs.go-jsonnet
    pkgs.golangci-lint
    pkgs.google-cloud-sdk
    pkgs.gopls
    pkgs.htop
    pkgs.httpie
    pkgs.hurl
    pkgs.ipatool
    pkgs.iperf
    pkgs.jq
    pkgs.just
    pkgs.kubectl
    pkgs.kustomize
    pkgs.lark-cli
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
    pkgs.python3Packages.markdown2
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
    pkgs.xonsh
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
  ]
  ++ pkgs.lib.mapAttrsToList (_: v: v) myScripts;
}
