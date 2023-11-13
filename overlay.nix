pkgs: hlib: helf: huper: {
  hnix-store-core =
    helf.callCabal2nix "hnix-store-core" ./hnix-store-core {};
  hnix-store-remote =
    if pkgs.stdenv.isDarwin
    then helf.callCabal2nix "hnix-store-remote" ./hnix-store-remote { }
    else
      ( helf.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" { }
      ).overrideAttrs (attrs: {
          buildInputs = attrs.buildInputs ++ [
            pkgs.nix
          ];
      });
}
