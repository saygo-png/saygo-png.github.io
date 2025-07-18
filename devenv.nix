{pkgs, ...}: {
  packages = [pkgs.hugo];
  languages.javascript.enable = true;
  languages.javascript.npm.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
