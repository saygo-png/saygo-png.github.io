{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [inputs.devenv.flakeModule];
      systems = nixpkgs.lib.systems.flakeExposed;

      perSystem = {
        # config,
        # self',
        # inputs',
        pkgs,
        # system,
        ...
      }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.
        devenv.shells.default = {
          # https://devenv.sh/reference/options/
          # pre-commit.hooks = {
          #   actionlint.enable = true;
          #   hunspell.enable = true;
          #   markdownlint.enable = true;
          # };
          processes = {
            hugo.exec = "hugo server --disableFastRender --noHTTPCache --ignoreCache";
          };
          dotenv.disableHint = true;
          packages = with pkgs; [hugo];
        };
      };
    };
}
