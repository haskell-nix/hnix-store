{ mkDerivation, attoparsec, base, containers, criterion, deepseq
, QuickCheck, stdenv, system-filepath, text, vector
}:
mkDerivation {
  pname = "nix-derivation";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base containers deepseq system-filepath text vector
  ];
  testHaskellDepends = [
    attoparsec base QuickCheck system-filepath text vector
  ];
  benchmarkHaskellDepends = [ attoparsec base criterion text ];
  description = "Parse and render *.drv files";
  license = stdenv.lib.licenses.bsd3;
}
