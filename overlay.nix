huper: helf: {
  hnix-store-core =
    helf.callCabal2nix "hnix-store-core" ./hnix-store-core {};
  hnix-store-remote =
    helf.callCabal2nix "hnix-store-remote" ./hnix-store-remote {};
  hnix-store-binary =
    helf.callCabal2nix "hnix-store-binary" ./hnix-store-binary {};
  hnix-store-s3 =
    helf.callCabal2nix "hnix-store-s3" ./hnix-store-s3 {};
}
