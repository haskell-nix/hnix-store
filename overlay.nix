huper: helf: {
  hnix-store-core =
    helf.callCabal2nix "hnix-store-core" ./hnix-store-core {};
  hnix-store-remote =
    helf.callCabal2nixWithOptions "hnix-store-remote" ./hnix-store-remote "-fio-testsuite" {};
}
