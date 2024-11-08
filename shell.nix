{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  packages = with pkgs; [
    hugo
    nodejs
    npm-check
    nodePackages.typescript
    nodePackages.typescript-language-server
  ];
}
