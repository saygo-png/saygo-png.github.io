{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    systems = {
      url = "path:./nix/systems.nix";
      flake = false;
    };
  };

  nixConfig.allow-import-from-derivation = false;

  outputs = {
    nixpkgs,
    systems,
    ...
  } @ inputs: let
    inherit (nixpkgs) lib;
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system:
      import nixpkgs {
        inherit system;
      });
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});
    treefmtEval = eachSystem (_: pkgs: inputs.treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix);
    getGhcPkgs = pkgs: pkgs.haskell.packages.ghc984;
  in {
    formatter = eachSystem (system: _: treefmtEval.${system}.config.build.wrapper);
    packages = eachSystem (_system: pkgs: let
      ghcPackages = getGhcPkgs pkgs;
      static-site-generator =
        pkgs.haskell.lib.compose.overrideCabal
        rec {
          # Only include needed files to prevent recompiling on unrelated changes
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./site.hs
              ./nix/cabal2nix-result.nix
              ./website-ssg.cabal
            ];
          };
          preConfigure = ''
            # Based on https://github.com/maralorn/nix-output-monitor/commit/f0554b1295703940f4a292962fde2b6d67301166
            echo "Checking that cabal2nix-result.nix is up-to-date."
            ${ghcPackages.cabal2nix}/bin/cabal2nix . > fresh-cabal2nix-result.nix
            echo ---
            cat ${src}/nix/cabal2nix-result.nix
            echo ---
            cat fresh-cabal2nix-result.nix
            echo ---
            ${pkgs.diffutils}/bin/diff -ws ${src}/nix/cabal2nix-result.nix fresh-cabal2nix-result.nix || (echo "Files differ!"; exit 1)
          '';
        }
        (ghcPackages.callPackage ./nix/cabal2nix-result.nix {});
    in rec {
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
          ghcPackages = getGhcPkgs pkgs;
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
