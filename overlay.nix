pkgs: hlib: helf: huper: {
  hnix-store-remote =
    ( helf.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" { }
    ).overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [
          pkgs.nix
        ];
    });
}
