{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    niceHaskell = {
      url = "github:saygo-png/nice-nixpkgs-haskell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    systems = {
      url = "path:./nix/systems.nix";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    systems,
    niceHaskell,
    self,
    ...
  } @ inputs: let
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system:
      import nixpkgs {
        inherit system;
      });
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});
    treefmtEval = eachSystem (_: pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
  in {
    formatter = eachSystem (system: _: treefmtEval.${system}.config.build.wrapper);
    packages = eachSystem (system: pkgs: rec {
      static-site-generator = pkgs.callPackage ./nix/package.nix {niceHaskell = niceHaskell.outputs.niceHaskell.${system};};
      website = pkgs.stdenv.mkDerivation {
        name = "my-website";
        src = ./.;

        buildPhase = ''
          cp -r --no-preserve=all $src/uncompiled-website .
          cd uncompiled-website
          ${nixpkgs.lib.getExe static-site-generator} build
        '';

        installPhase = ''
          mkdir -p $out
          cp -r _site/. $out
        '';
      };
      ssg = static-site-generator;
      default = website;
    });
    devShells = eachSystem (_system: pkgs: {
      default = pkgs.mkShell {
        packages = let
          ghcPackages = pkgs.haskell.packages.ghc984;
        in [
          pkgs.pkg-config
          pkgs.libsodium
          pkgs.zlib
          ghcPackages.ghc
          ghcPackages.cabal-install
          ghcPackages.haskell-language-server
        ];
      };
    });
  };
}
