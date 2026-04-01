{
  description = "Home environment flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/cf87881886182975bef15495007d6580c4aa6450";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/35e79fe95d7cec6365a08e3759819420e89b73f2";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    kimi-cli = {
      url = "github:MoonshotAI/kimi-cli/1.24.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    git = {
      url = "path:./lib/git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jj = {
      url = "path:./lib/jj";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git.follows = "git";
    };
    notmuch = {
      url = "path:./lib/notmuch";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ripgrep = {
      url = "path:./lib/ripgrep";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    my-scripts = {
      url = "path:./lib/my-scripts";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agent-tools = {
      url = "path:./lib/agent-tools";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.my-scripts.follows = "my-scripts";
      inputs.kimi-cli.follows = "kimi-cli";
    };
    aider = {
      url = "path:./lib/aider";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.git.follows = "git";
      inputs.my-scripts.follows = "my-scripts";
    };
  };

  outputs = { self, nixpkgs, emacs-overlay, git, jj, notmuch, ripgrep, my-scripts, agent-tools, aider, ... }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.doCheckByDefault = false;
            config.allowUnfree = true;
            overlays = [ emacs-overlay.overlays.default ];
          };
        in
        {
          home = pkgs.callPackage ./home.nix {
            git = git.packages.${system}.default;
            jj = jj.packages.${system}.default;
            notmuch = notmuch.packages.${system}.default;
            ripgrep = ripgrep.packages.${system}.default;
            my-scripts = my-scripts.packages.${system}.default;
            agent-tools = agent-tools.packages.${system}.default;
            aider = aider.packages.${system}.default;
          };
          default = self.packages.${system}.home;
        }
      );
    };
}
