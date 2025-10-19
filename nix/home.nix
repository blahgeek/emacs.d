{ config, pkgs, ... }:

{
  nixpkgs.config.doCheckByDefault = false;

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

    pkgs.ripgrep
    pkgs.aider-chat
    pkgs.bazel-buildtools  # buildifier
    pkgs.cpplint
    pkgs.fd
    pkgs.glab
    pkgs.pyright
    pkgs.uv
    pkgs.python312Packages.httpie
    pkgs.python312Packages.markdown2

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
  };

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
