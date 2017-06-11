{ mkDerivation, base, base16-bytestring, bytestring, filepath
, process, resourcet, setenv, sqlite-simple, stdenv, temporary
, text, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "hnix-store";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base16-bytestring bytestring resourcet sqlite-simple text time
    unordered-containers
  ];
  executableHaskellDepends = [
    base base16-bytestring bytestring filepath process resourcet setenv
    sqlite-simple temporary text time transformers unordered-containers
  ];
  homepage = "https://github.com/shlevy/hnix-store";
  description = "Haskell implementation of the nix store API";
  license = stdenv.lib.licenses.mit;
}
