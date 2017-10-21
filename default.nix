{ mkDerivation, attoparsec, base, containers, deepseq, pretty-show
, QuickCheck, stdenv, system-filepath, text, vector
}:
mkDerivation {
  pname = "nix-derivation";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers deepseq system-filepath text vector
  ];
  executableHaskellDepends = [ attoparsec base pretty-show text ];
  testHaskellDepends = [
    attoparsec base QuickCheck system-filepath text vector
  ];
  description = "Parse and render *.drv files";
  license = stdenv.lib.licenses.bsd3;
}
