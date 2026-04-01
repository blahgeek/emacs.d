{ pkgs
, git
, jj
, notmuch
, ripgrep
, my-scripts
, agent-tools
, aider
}:

let
  emacs = pkgs.emacs-git-nox.override {
    withNativeCompilation = true;
    withSelinux = false;
    withSystemd = false;
    withCompressInstall = false;
  };

  python312Custom = pkgs.python312.override {
    packageOverrides = final: prev: {
      xonsh = prev.xonsh.overridePythonAttrs (old: {
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

  xonshCustom = pkgs.xonsh.override {
    python3 = python312Custom;
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
        build-system = [ setuptools ];
      })
    ];
  };

  emacs-lsp-booster = pkgs.rustPlatform.buildRustPackage rec {
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
  };

  librime-custom = pkgs.librime.override {
    plugins = [
      (pkgs.librime-lua.override {
        lua = pkgs.lua5_4;
      })
    ];
  };

  curl-custom = pkgs.curl.override { c-aresSupport = !pkgs.stdenv.isDarwin; };
  rclone-custom = pkgs.rclone.override { enableCmount = pkgs.stdenv.isDarwin; };
in
pkgs.buildEnv {
  name = "home";
  pathsToLink = [ "/bin" "/etc" "/include" "/lib" "/libexec" "/sbin" "/share" ];
  paths = [
    emacs
    pkgs.ast-grep
    pkgs.autojump
    pkgs.bazel-buildtools
    pkgs.bazelisk
    pkgs.bind.dnsutils
    pkgs.bubblewrap
    pkgs.clang-tools
    pkgs.clickhouse
    pkgs.cpplint
    curl-custom
    pkgs.docker-client
    pkgs.docker-compose
    pkgs.dtrx
    pkgs.fd
    pkgs.file
    pkgs.ffmpeg
    pkgs.flamegraph
    pkgs.fzf
    pkgs.gawk
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
    rclone-custom
    librime-custom
    pkgs.rime-ice
    xonshCustom
    emacs-lsp-booster
    git
    jj
    notmuch
    notmuch.emacs
    ripgrep
    my-scripts
    agent-tools
    aider
  ];
}
