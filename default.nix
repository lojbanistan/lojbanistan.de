with import <nixpkgs> {};

haskellPackages.override {
  overrides = self: super: {
    lojbanistan-de = self.callPackage ./lojbanistan-de.nix {};
  };
}
