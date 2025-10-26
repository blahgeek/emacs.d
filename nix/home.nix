{ config, pkgs, ... }:

let
  _app_dir = "Applications/HomeManager";
in
{
  nixpkgs.config.doCheckByDefault = false;
  nixpkgs.config.allowUnfree = true;

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
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    pkgs.aider-chat
    pkgs.autojump
    pkgs.bash
    pkgs.bash-completion
    pkgs.bazel-buildtools  # buildifier
    pkgs.bazelisk
    pkgs.bind.dnsutils
    pkgs.claude-code
    pkgs.coreutils-prefixed
    pkgs.cpplint
    pkgs.curl
    pkgs.docker-client
    pkgs.docker-compose
    pkgs.dtrx
    pkgs.fd
    pkgs.ffmpeg-headless
    pkgs.flamegraph
    pkgs.gawk  # install as awk directly
    pkgs.git
    pkgs.git-lfs
    pkgs.glab
    pkgs.gnupg
    pkgs.go
    pkgs.go-jsonnet
    pkgs.golangci-lint
    pkgs.google-cloud-sdk
    pkgs.gopls
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
    pkgs.pass
    pkgs.passExtensions.pass-otp
    pkgs.pv
    pkgs.pwgen
    pkgs.pyright
    pkgs.python312Packages.httpie
    pkgs.python312Packages.markdown2
    pkgs.rclone
    pkgs.ripgrep
    pkgs.rsync
    pkgs.tmux
    pkgs.unixtools.watch
    pkgs.unrar
    pkgs.uv
    pkgs.w3m-nox
    pkgs.yubikey-manager
    pkgs.yubikey-personalization

    # sed, but install as gsed
    (pkgs.linkFarm "gnused-prefixed" [
      { name = "bin/gsed"; path = "${pkgs.gnused}/bin/sed"; }
    ])

    (pkgs.xonsh.override {
      python3 = pkgs.python312;
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

    (pkgs.rustPlatform.buildRustPackage rec {
      pname = "jujutsu";
      version = "0.34.0-lfs";
      src = pkgs.fetchFromGitHub {
        owner = "blahgeek";
        repo = "jujutsu";
        rev = "a7d98a0564ecaccad970473c88083fba0352236c";  # lfs-0.34.0
        hash = "sha256-khwwcTxwsLUJyb11Ityoqn3SO3rUwFaQ5AByJMbSxow=";
      };
      cargoHash = "sha256-/zC2z0evYs8VKta0uClTtl4l3tbDRcsVedGF2jtfQGA=";

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
    pkgs.pinentry_mac
  ] else [])
  ++
  [
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
  ];

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
