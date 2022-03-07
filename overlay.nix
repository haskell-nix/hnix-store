pkgs: hlib: helf: huper: {
  hnix-store-remote =
    ( helf.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" { relude = helf.relude_1_0_0_1; }
    ).overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          pkgs.nix
        ];
    });
}
