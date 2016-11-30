{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  version = "0.1";

  # src = with lib; traceVal ( flip builtins.filterSource ./. (f: _:
  #         (all (pfx: baseNameOf f != pfx)
  #         # filter those out
  #         [ "_site" "_cache" "dist" "release.nix" ])));
  src = ./.;


  # generate .nix from cabal file
  nixfile = lib.traceVal (runCommand "lojbanistan-de.nix" {} ''
    ${haskellPackags.cabal2nix}/bin/cabal2nix ./. > $out
  '');

  hp = haskellPackages.override {
    overrides = self: super: {
      # hakyll site generator
      lojbanistan-sitegen = super.callPackage "${nixfile}" {};
    };
  };
  # bin = "${hp.lojbanistan-sitegen}/bin/lojbanistan-de";

in
stdenv.mkDerivation {
  name = "lojbanistan.de-${version}";

  inherit src;

  buildPhase = ''
    # HIER HÄNGT ER SICH AUF
    echo ${hp.lojbanistan-sitegen} build
  '';
  # installPhase = ''
  #   mkdir $out
  #   echo cp -r ./_site/* $out
  # '';
}
