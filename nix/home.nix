{
  config,
  # pkgs,
  ...
}:

let
  # https://github.com/NixOS/nixpkgs/commit/a7fc11be66bdfb5cdde611ee5ce381c183da8386
  # 2025.12.14
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/6c84b83832130abaf2b053580254289f90f6aaec.tar.gz") {
    config.doCheckByDefault = false;
    config.allowUnfree = true;
    overlays = [
      (import (builtins.fetchTarball {
        # 2025.12.14
        url = "https://github.com/nix-community/emacs-overlay/archive/6348314e322d12487f3abb6282d4612fe8d93fe4.tar.gz";
      }))
    ];
  };
  _app_dir = "Applications/HomeManager";
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  targets.darwin.linkApps.directory = _app_dir;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    pkgs.ast-grep
    pkgs.autojump
    pkgs.bazel-buildtools  # buildifier
    pkgs.bazelisk
    pkgs.bind.dnsutils
    pkgs.claude-code
    pkgs.clickhouse
    pkgs.cpplint
    (pkgs.curl.override { c-aresSupport = !pkgs.stdenv.isDarwin; })
    pkgs.docker-client
    pkgs.docker-compose
    pkgs.dtrx
    pkgs.fd
    pkgs.ffmpeg
    pkgs.flamegraph
    pkgs.fzf
    pkgs.gawk  # install as awk directly
    pkgs.gemini-cli
    pkgs.git
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
    pkgs.kubectl
    pkgs.less
    pkgs.moreutils
    pkgs.mtr
    pkgs.ncdu
    pkgs.neovim
    pkgs.nmap
    pkgs.notmuch
    pkgs.notmuch.emacs
    pkgs.pre-commit
    pkgs.pv
    pkgs.pwgen
    pkgs.pyright
    pkgs.python312Packages.httpie
    pkgs.python312Packages.markdown2
    pkgs.ripgrep
    pkgs.rsync
    pkgs.rustup
    pkgs.socat
    pkgs.time
    pkgs.tmux
    pkgs.typos-lsp
    pkgs.unrar
    pkgs.uv
    pkgs.w3m-nox
    pkgs.yubikey-manager
    pkgs.yubikey-personalization

    # yes, do not enable mount in linux.
    # when enabled, it would use "fusermount3" in nix, which does not have setuid bit set.
    # when disabled, the feature is still present, but it would use system's fusermount3
    (pkgs.rclone.override { enableCmount = pkgs.stdenv.isDarwin; })

    (pkgs.librime.override {
      plugins = [
        (pkgs.librime-lua.override {
          # https://github.com/iDvel/rime-ice/issues/840
          lua = pkgs.lua5_4;
        })
      ];
    })
    pkgs.rime-ice

    (
      # patch GitPython to support git index v3, which would produced by jujutsu
      # https://github.com/Aider-AI/aider/issues/211
      # https://github.com/gitpython-developers/GitPython/pull/2081
      let python312 = pkgs.python312.override {
            packageOverrides = final: prev: rec {
              gitpython = prev.gitpython.overridePythonAttrs (prev: rec {
                version = "3.1.45-index-v3-support-master";
                src = pkgs.fetchFromGitHub {
                  owner = "gitpython-developers";
                  repo = "GitPython";
                  rev = "564a043413a451356f64c51be6556fb7266000a1";
                  hash = "sha256-Aeqlzz1iDrZzU4jTUr483n3dZCI1ukzzCx/iUXGugBw=";
                };
              });
            };
          };
      in
        (pkgs.aider-chat.override {
          python312Packages = python312.pkgs;
        }).overrideAttrs (prev : {
          doCheck = false;
        })
    )

    (
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
        (pkgs.xonsh.override {
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
    )

    (pkgs.rustPlatform.buildRustPackage rec {
      pname = "jujutsu";
      version = "0.35.0-lfs";
      src = pkgs.fetchFromGitHub {
        owner = "blahgeek";
        repo = "jujutsu";
        rev = "7e3319817cebe90b0ed510ea5ebb5af13578e123";  # lfs-0.35.0
        hash = "sha256-ns4fmh9Cl0qGzdluBl0Ucex35eWSUOEV7w47vkQqjw0=";
      };
      cargoHash = "sha256-4XqYHpLtaNrjKYsloBmB03X+X7DWrsR5mWO74i9cs9M=";

      doCheck = false;
      cargoBuildFlags = [
        # Don’t install the `gen-protos` build tool.
        "--bin"
        "jj"
      ];
    })

    (pkgs.rustPlatform.buildRustPackage rec {
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
    })
  ]
  ++
  (if pkgs.stdenv.isDarwin then [
    pkgs.bash
    pkgs.bash-completion
    pkgs.coreutils-prefixed
    pkgs.pinentry_mac
    pkgs.unixtools.watch

    # sed, but install as gsed
    (pkgs.linkFarm "gnused-prefixed" [
      { name = "bin/gsed"; path = "${pkgs.gnused}/bin/sed"; }
    ])


    # fonts! share/fonts/ would automatically be installed into ~/Library/Fonts/HomeManager/
    pkgs.twemoji-color-font  # this is SVGinOT font, not twitter-color-emoji

    (pkgs.stdenv.mkDerivation {
      pname = "my-fonts";
      version = "e06cb2f";
      src = builtins.fetchGit {  # apparently only the builtins version can use ssh credential
        url = "git@github.com:blahgeek/PragmataPro.git";
        rev = "e06cb2fda8a85905ff327d4baf9d7e4b4f81e352";
        shallow = true;
        lfs = true;
      };

      dontBuild = true;
      installPhase = ''
        mkdir -p $out/share/fonts/truetype/
        mkdir -p $out/share/fonts/opentype/
        cp -r $src/0830/PragmataPro*.ttf $out/share/fonts/truetype/
        cp -r $src/cnfonts/HYQiHei*.otf $out/share/fonts/opentype/
      '';
    })
  ] else []) ++
  (if pkgs.stdenv.isLinux then [
    (pkgs.emacs-git-nox.override {
      withNativeCompilation = true;
      withSelinux = false;
      withSystemd = false;
      withCompressInstall = false;
    })
  ] else []);

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  } // (if pkgs.stdenv.isDarwin then {
    ".gnupg/gpg-agent.conf".text = ''
      pinentry-program ${builtins.getEnv "HOME"}/${_app_dir}/pinentry-mac.app/Contents/MacOS/pinentry-mac
    '';
  } else {});

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/blahgeek/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
