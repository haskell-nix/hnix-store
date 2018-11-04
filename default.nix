{ mkDerivation, attoparsec, base, containers, criterion, deepseq
, pretty-show, QuickCheck, stdenv, system-filepath, text, vector
}:
mkDerivation {
  pname = "nix-derivation";
  version = "1.0.2";
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
  benchmarkHaskellDepends = [ attoparsec base criterion text ];
  description = "Parse and render *.drv files";
  license = stdenv.lib.licenses.bsd3;
}
