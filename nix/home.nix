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
  lib = origPkgs.lib;

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

    lark-cli = (pkgs.buildGoModule {
      name = "lark-cli";
      src = sources.lark-cli;
      vendorHash = "sha256-CI2RGE6jSxsY9LUxPvG350HRPksme8or4O+9SLi4wOY=";
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
      cargoHash = "sha256-qchwxW3KITQcv6EFzR2BSISWB2aTW9EdCN/bx5m0l48=";
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
    runtimeInputs = [ myScripts.emacs-get-gptel-api-key myScripts.emacsclient-on-current-server ];
    bashOptions = [];  # "errexit" "nounset" "pipefail"
    text = ''
      ${pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (k: v: "export ${k}=${v}") envs)}
      export SKILLS_DIR=${agentSkills}
      exec ${./etc/agent-tools}/sandbox-run ${./etc/agent-tools}/${name}.bash ${pkg}/bin/${name} "$@"
    '';
  });

  agentSkills = pkgs.symlinkJoin {
    name = "agent-skills";
    paths = [
      ./etc/agent-skills
      (pkgs.buildEnv {
        name = "lark-cli-skills-trimmed";
        paths = [ "${sources.lark-cli}/skills" ];
        pathsToLink = [
          "/lark-doc"
          "/lark-drive"
          "/lark-contact"
          "/lark-im"
          "/lark-openapi-explorer"
          "/lark-shared"
          "/lark-whiteboard"
          "/lark-wiki"
        ];
      })
      (pkgs.buildEnv {
        name = "agent-browser-skills-trimmed";
        paths = [ "${pkgs.agent-browser}/share/agent-browser/skills" ];
        pathsToLink = [
          "/agent-browser"
        ];
      })
    ];
  };
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
        runtimeInputs = [];
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
        (let
          script = uv2nix.lib.scripts.loadScript { script = ./etc/my-scripts/${name}; };
          overlay = script.mkOverlay { sourcePreference = "wheel"; };
          pythonSet = (pkgs.callPackage pyproject-nix.build.packages {
            python = pkgs.python312;
          }).overrideScope overlay;
        in {
          name = name;  # keep ".py" in name
          value = pkgs.writeScriptBin name (
            script.renderScript {
              venv = script.mkVirtualEnv { inherit pythonSet; };
            }
          );
        }))
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

    (pkgs.emacs-git-nox.override {
      withNativeCompilation = true;
      withSelinux = false;
      withSystemd = false;
      withCompressInstall = false;
    })

    (mkAgentTool "claude" pkgs.claude-code {})
    (mkAgentTool "codex" pkgs.codex {})
    (mkAgentTool "gemini" pkgs.gemini-cli {})
    (mkAgentTool "kimi" pkgs.kimi-cli {})
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

    (mkWrapperWithEnv "agent-browser" pkgs.agent-browser {
      AGENT_BROWSER_EXECUTABLE_PATH = (
        let chromeDir =
              {
                x86_64-linux = "chrome-linux64";
                aarch64-linux = "chrome-linux";
              }.${pkgs.stdenv.hostPlatform.system};
        in "${pkgs.playwright.browsers-chromium}/chromium-${pkgs.playwright.browsersJSON.chromium.revision}/${chromeDir}/chrome");
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
