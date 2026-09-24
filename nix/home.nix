{...}:

let
  sources = import ./sources.nix;

  pkgs = origPkgs // myPkgs;
  origPkgs = import sources.nixpkgs {
    config.doCheckByDefault = false;
    config.allowUnfree = true;
  };
  lib = origPkgs.lib;

  # To update WebBridge: choose a release from latest/version.json, set this
  # version, copy the two binary hashes from that manifest, and recompute the
  # skill fetchzip hash below with `nix-prefetch-url --unpack <skill-url>`.
  kimiWebbridgeVersion = "v2.0.5";

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

    kimi-webbridge = let
      release =
        if !pkgs.stdenv.hostPlatform.isLinux then
          throw "kimi-webbridge is only packaged for Linux"
        else if pkgs.stdenv.hostPlatform.isAarch64 then {
          arch = "arm64";
          hash = "sha256-oR2EdhiCWC4pF2WnGt76V/p98PRgy7Uah75UyuKb9ks=";
        } else if pkgs.stdenv.hostPlatform.isx86_64 then {
          arch = "amd64";
          hash = "sha256-J8zLe7ApxaXc05YwHSJhOU0sKkPmG/C6cyE38i/1u9E=";
        } else
          throw "kimi-webbridge is only packaged for Linux aarch64 and x86_64";
    in pkgs.stdenvNoCC.mkDerivation {
      pname = "kimi-webbridge";
      version = lib.removePrefix "v" kimiWebbridgeVersion;
      src = pkgs.fetchurl {
        url = "https://cdn.kimi.com/webbridge/${kimiWebbridgeVersion}/releases/kimi-webbridge-linux-${release.arch}";
        inherit (release) hash;
      };
      dontUnpack = true;
      installPhase = ''
        install -Dm755 "$src" "$out/bin/kimi-webbridge"
      '';
    };

    kimi-webbridge-skill = let
      src = pkgs.fetchzip {
        url = "https://cdn.kimi.com/webbridge/${kimiWebbridgeVersion}/skills/kimi-webbridge.tar.gz";
        hash = "sha256-cxVc/DXzIcV1M8b2fzpTyGIu2jVuM4mevyIWtnoq5V4=";
        stripRoot = true;
      };
    in pkgs.runCommand "kimi-webbridge-skill" {} ''
      mkdir $out
      cp -a ${src}/. $out/kimi-webbridge
      chmod -R u+w $out
      for f in $(find $out -type f -name '*.md'); do
        substituteInPlace "$f" \
          --replace-fail '~/.kimi-webbridge/bin/kimi-webbridge' 'kimi-webbridge'
      done
    '';

    pi-coding-agent = (let
      piNix = sources.pi-nix;
      current = builtins.fromJSON (builtins.readFile (piNix + "/VERSION.json"));
      version = lib.removePrefix "v" current.rev;
      src = origPkgs.fetchFromGitHub {
        owner = "earendil-works";
        repo = "pi";
        rev = current.rev;
        hash = current.hash;
      };
    in
      origPkgs.callPackage (piNix + "/coding-agent/package.nix") {
        inherit src version;
        npmDepsHash = current.projects.coding-agent.npmDepsHash;
      });

    lark-cli = (pkgs.buildGoModule {
      name = "lark-cli";
      src = sources.lark-cli;
      vendorHash = "sha256-WClES7ilNmQ0018Qf13tNHouE/SIwh99MaewZ7VGQ2E=";
      subPackages = [ "." ];
      doCheck = false;
    }).overrideAttrs(old: {
      postInstall = (old.postInstall or "") + ''
        mv $out/bin/cli $out/bin/lark-cli
      '';
    });

    emacs-lsp-booster = (pkgs.rustPlatform.buildRustPackage rec {
      pname = "emacs-lsp-booster";
      version = sources.emacs-lsp-booster.rev;
      src = sources.emacs-lsp-booster;
      cargoHash = "sha256-7lIceMT2hJplHU2VIN1O8IiGE6+DxO4/uM8pYS/qvlE=";
      doCheck = false;
    });

    # notmuch 0.40 has a few flaky test failures (emacs crypto tests) in the
    # sandboxed build env; skip its test suite.
    notmuch = origPkgs.notmuch.overrideAttrs (old: {
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

  # make treesit grammars available under lib/ for emacs.
  # Emacs assumes the dynamic library for LANG is libtree-sitter-LANG.EXT
  mkTreesitGrammar = name :
    (let t = pkgs.tree-sitter-grammars."tree-sitter-${name}";
     in
       pkgs.runCommand "treesit-grammar-${name}" {} ''
        mkdir -p $out/lib/
        ln -s ${t}/parser $out/lib/libtree-sitter-${name}.so
      '');

  # copy entire folder, replace @@@ to each file's dir path
  mkConfigDir = dir: pkgs.runCommand "config-${builtins.baseNameOf dir}" {} ''
    cp -ar ${dir} $out
    chmod -R +w $out
    # NOTE: double quote before dollar below is used for escaping inside nix string literal
    find $out -type f -exec bash -c 'sed -i "s|@@@|''${1%/*}|g" "$1"' _ {} \;
  '';

  myScripts =
    builtins.mapAttrs
      (name: _: pkgs.writeShellApplication {
        name = name;
        runtimeInputs = [ pkgs.jq pkgs.curl ];  # set some common tools
        runtimeEnv = if lib.filesystem.pathIsDirectory ./etc/my-scripts/${name}.d then {
          _SCRIPT_DATA_DIR = "${./etc/my-scripts/${name}.d}";
        } else {};
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

  home.packages =
    (let
      skills = pkgs.symlinkJoin {
        name = "agent-skills";
        paths = [
          ./etc/agent-skills
          (pkgs.buildEnv {
            name = "lark-cli-skills-trimmed";
            paths = [ "${sources.lark-cli}/skills" ];
            pathsToLink = [
              "/lark-base"
              "/lark-doc"
              "/lark-drive"
              "/lark-im"
              "/lark-mail"
              "/lark-contact"
              "/lark-openapi-explorer"
              "/lark-shared"
              "/lark-whiteboard"
              "/lark-wiki"
            ];
          })
          # I don't like agent-browser, it bundles many other skills
          # see playwright-cli below
          (pkgs.runCommand "playwright-cli-skills" {} ''
            mkdir -p $out
            ln -s ${pkgs.playwright}/lib/tools/cli-client/skill $out/playwright-cli
          '')
          pkgs.kimi-webbridge-skill
        ];
      };
      deps = [
        # some tools for agent that should use different configs then for me
        (mkWrapperWithEnv "git" pkgs.git {
          GIT_CONFIG_GLOBAL = "${mkConfigDir ./etc/git}/config-agent";
        })
        # Single-user build (DROPBEAR_SVR_MULTIUSER=0) for running in an
        # unprivileged userns container where only one uid is mapped and
        # setgroups(2) is denied.
        (pkgs.dropbear.overrideAttrs (old: {
          patches = (old.patches or []) ++ [
            ./patches/dropbear-single-user-userns.patch
          ];
        }))
        # prevent nesting
        pkgs.pi-coding-agent
      ];
      piAgent = pkgs.runCommand "pi-agent" {} ''
        mkdir -p $out

        ln -s ${skills} $out/skills
        ln -s ${./etc/pi/extensions/src} $out/extensions
        ln -s ${./etc/pi/keybindings.json} $out/keybindings.json
      '';
      piAgentReadonly = pkgs.runCommand "pi-agent" {} ''
        mkdir -p $out/extensions

        ln -s ${./etc/pi/extensions/src}/stealth-provider.ts $out/extensions/
        ln -s ${./etc/pi/keybindings.json} $out/keybindings.json
      '';
      wrapper = pkgs.writeShellScript "pi-run.sh" (builtins.readFile ./etc/pi/run.sh);
    in [
      (
        pkgs.writeShellApplication {
          name = "pi";
          runtimeInputs = deps;
          runtimeEnv = {
            PI_CODING_AGENT_DIR = piAgent;
            PI_REQUIRED_APIKEYS = ''
                KIMI_API_KEY:code.kimi.com
                TAVILY_API_KEY:api.tavily.com
                GH_TOKEN:api.github.com:blahgeek^agent-ro
            '';
          };
          text = ''exec ${wrapper} --append-system-prompt ${./etc/pi/sp-append.md} "$@"'';
        }
      )
      (
        pkgs.writeShellApplication {
          name = "pi-readonly";
          runtimeInputs = deps;
          runtimeEnv = {
            PI_CODING_AGENT_DIR = piAgentReadonly;
          };
          text = ''exec ${wrapper} --tools read,grep,find,ls "$@"'';
        }
      )
    ]
  ) ++ [
    (mkWrapperWithEnv "git" pkgs.git {
      GIT_CONFIG_GLOBAL = "${mkConfigDir ./etc/git}/config";
    })
    (mkWrapperWithEnv "rg" pkgs.ripgrep {
      RIPGREP_CONFIG_PATH = ./etc/ripgrep/ripgrep.config;
    })
    (
      # https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages
      # If only `wrapped` is used, then its manpages etc., are buried
      let wrapped = pkgs.writeShellApplication {
            name = "jj";
            runtimeInputs = [ pkgs.jujutsu pkgs.git pkgs.fzf ];
            text = ''
                export GIT_CONFIG_GLOBAL=${"${mkConfigDir ./etc/git}/config"}
                export JJ_CONFIG=${"${mkConfigDir ./etc/jj}/config.toml"}:~/.config/jj/config.toml
                exec jj "$@"
            '';
          }; in pkgs.symlinkJoin {
            name = "jj";
            paths = [ wrapped pkgs.jujutsu ];
          }
    )
    (
      let wrapped = (pkgs.writeShellScriptBin "fish" ''
        _FISH_CONFIG=${mkConfigDir ./etc/fish}
        exec ${pkgs.fish}/bin/fish -C "source $_FISH_CONFIG/config.fish" "$@"
      ''); in pkgs.symlinkJoin {
        name = "fish";
        paths = [ wrapped pkgs.fish ];
      }
    )

    (mkWrapperWithEnv "notmuch" pkgs.notmuch {
      NOTMUCH_CONFIG = "${mkConfigDir ./etc/notmuch}/config";
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

    pkgs.aliyun-cli
    pkgs.ast-grep
    pkgs.autojump
    pkgs.bash
    pkgs.bazel-buildtools  # buildifier
    pkgs.bazelisk
    pkgs.bc
    pkgs.bind.dnsutils
    pkgs.binutils
    pkgs.bitwarden-cli
    pkgs.bubblewrap
    (pkgs.busybox.override { enableAppletSymlinks = false; })
    pkgs.cargo-sweep
    pkgs.clang-tools
    pkgs.coreutils
    # cpplint 2.0.2 tests fail on python 3.14 (DeprecationWarning in output breaks assertions)
    # NOTE: python packages map doCheck->doInstallCheck, so override doInstallCheck directly
    (pkgs.cpplint.overrideAttrs (old: { doInstallCheck = false; }))
    ((pkgs.curl.override { c-aresSupport = !pkgs.stdenv.isDarwin; }).overrideAttrs (old: {
      configureFlags = old.configureFlags ++ [ "--enable-ssls-export" ];
      # ssl cookie export, to support tls 0-RTT across commands
    }))
    pkgs.docker-client
    pkgs.docker-compose
    pkgs.dtrx
    pkgs.emacs
    pkgs.emacs-lsp-booster
    pkgs.fd
    pkgs.ffmpeg
    pkgs.file
    pkgs.findutils
    pkgs.flamegraph
    pkgs.fzf
    pkgs.gawk  # install as awk directly
    pkgs.gdb
    pkgs.gh
    pkgs.ghostty.terminfo
    pkgs.git-lfs
    pkgs.glab
    pkgs.gnugrep
    pkgs.gnupg
    pkgs.gnused
    pkgs.gnutar
    pkgs.go
    pkgs.go-jsonnet
    pkgs.golangci-lint
    pkgs.google-cloud-sdk
    pkgs.gopls
    pkgs.gzip
    pkgs.htop
    pkgs.httpie
    pkgs.hurl
    pkgs.ipatool
    pkgs.iperf
    pkgs.iproute2
    pkgs.jq
    pkgs.just
    pkgs.kimi-webbridge
    pkgs.kubectl
    pkgs.kubectl-node-shell
    pkgs.kustomize
    pkgs.lark-cli
    pkgs.less
    pkgs.lsof
    pkgs.ltrace
    pkgs.moreutils
    pkgs.mtr
    pkgs.mutagen
    pkgs.ncdu
    pkgs.neovim
    pkgs.niv
    pkgs.nmap
    pkgs.nodejs
    pkgs.offlineimap
    pkgs.patch
    pkgs.pre-commit
    pkgs.procps
    pkgs.psmisc
    pkgs.pv
    pkgs.pwgen
    pkgs.pyright
    pkgs.python3Packages.markdown2
    # yes, do not enable mount in linux.
    # when enabled, it would use "fusermount3" in nix, which does not have setuid bit set.
    # when disabled, the feature is still present, but it would use system's fusermount3
    (pkgs.rclone.override { enableCmount = pkgs.stdenv.isDarwin; })
    pkgs.regclient
    pkgs.rsync
    pkgs.rustup
    pkgs.slirp4netns
    pkgs.socat
    pkgs.strace
    pkgs.tcpdump
    pkgs.time
    pkgs.tmux
    # pkgs.typescript is now the golang version: https://devblogs.microsoft.com/typescript/typescript-native-port/
    # it should replaces both pkgs.typescript_5 and pkgs.typescript-language-server
    # however, emacs lsp-mode does not support tsgo well enough for now: https://github.com/emacs-lsp/lsp-mode/issues/5081
    # let's use legacy typescript for now
    pkgs.typescript_5
    pkgs.typescript-language-server
    pkgs.typos-lsp
    pkgs.unrar
    pkgs.unzip
    pkgs.util-linux
    pkgs.uv
    pkgs.vmtouch
    pkgs.wget
    pkgs.whois
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
  ]

  ++ pkgs.lib.map mkTreesitGrammar [
    "bash"
    "c"
    "c-sharp"
    "clojure"
    "cmake"
    "cpp"
    "css"
    "dart"
    "dockerfile"
    "elisp"
    "elixir"
    "glsl"
    "go"
    "gomod"
    "haskell"
    "heex"
    "html"
    "java"
    "javascript"
    "json"
    "kotlin"
    "lua"
    "make"
    "markdown"
    "markdown-inline"
    "nix"
    "org"
    "perl"
    "php"
    "proto"
    "python"
    "ruby"
    "rust"
    "scala"
    "scss"
    "sql"
    "toml"
    "tsx"
    "typescript"
    "typst"
    "wgsl"
    "yaml"
    "zig"
  ]

  ++ pkgs.lib.mapAttrsToList (_: v: v) myScripts;

  # https://wiki.nixos.org/wiki/Apropos
  # 'fish' rely on this to complete 'man' cmd
  programs.man.generateCaches = true;
}
