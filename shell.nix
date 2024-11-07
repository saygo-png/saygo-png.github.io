{
pkgs ? import <nixpkgs> {},
unstable-pkgs ? import <nixpkgs-unstable> {}
}:
  pkgs.mkShell {
    packages = [
      unstable-pkgs.deno
      pkgs.hugo
    ];
  }
