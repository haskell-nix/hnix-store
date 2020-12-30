pkgs: hlib: helf: huper: {
  hnix-store-core =
    helf.callCabal2nix "hnix-store-core" ./hnix-store-core {};
  hnix-store-remote =
    (helf.callCabal2nixWithOptions
      "hnix-store-remote"
      ./hnix-store-remote
      "-fio-testsuite"
      {}
    ).overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ [ pkgs.nix pkgs.which ];
    });
  #  2020-12-30: NOTE: Remove after switch from cryptohash
  cryptohash-sha512 =
    hlib.unmarkBroken ( hlib.doJailbreak huper.cryptohash-sha512 );
}
