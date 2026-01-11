{niceHaskell, ...}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./..;
  cabalName = "drug2";
  compiler = "ghc984";
  # developPackageArgs.overrides = _: super: {
  #   say = pkgs.haskell.lib.dontCheck super.say;
  # };
}
