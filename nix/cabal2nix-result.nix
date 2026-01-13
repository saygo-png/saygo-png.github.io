{ mkDerivation, base, filepath, hakyll, hashable, lib, path, time
, unordered-containers
}:
mkDerivation {
  pname = "website-ssg";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base filepath hakyll hashable path time unordered-containers
  ];
  license = "unknown";
  mainProgram = "site";
}
