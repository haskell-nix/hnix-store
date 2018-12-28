let

  # TODO pin nixpkgs
  pkgs0 = import <nixpkgs>;

  hsOverrides = self: super: {
    hnix-store-core   = pkgs.haskellPackages.callCabal2nix "hnix-store-core"   ./hnix-store-core {};
    hnix-store-remote = pkgs.haskellPackages.callCabal2nix "hnix-store-remote" ./hnix-store-remote {};
  };

  pkgs = pkgs0 {
    overlays = [ (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = hsOverrides;
      };
    }) ];
  };

  # pkgs = pkgs0.override {
  #   overrides = self: super: {
  #     haskellPackages = self.haskellPackages.override {
  #       overrides = hsOverrides;
  #     };
  #   };
  # };

in
{
  hnix-store-core   = pkgs.haskellPackages.hnix-store-core;
  hnix-store-remote = pkgs.haskellPackages.hnix-store-remote;
}
