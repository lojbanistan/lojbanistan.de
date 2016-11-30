{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let
  version = "0.1";

  src = with lib;
    let cacheFilter = f: (all (pfx: baseNameOf f != pfx)
          # filter those out
          [ "_site" "_cache" "dist" "release.nix" ]);
    in traceVal (builtins.filterSource
                (f: t: cacheFilter f && cleanSourceFilter f t)
                ./.);

  jbovlaste = fetchFromGitHub {
    owner = "lojbanistan";
    repo = "jbovlaste-dicts";
    rev = "6660473b38da3dfa8a9c75febfd3dbb7f7ff5f0a";
    sha256 = "1483y3il0r2hw5ylkxpz0p7ga8x1vwi3migyirhkgan2gznqyk48";
  };

  # generate .nix from cabal file
  nixfile = runCommand "lojbanistan-de.nix" {} ''
    ${haskellPackages.cabal2nix}/bin/cabal2nix ${src} > $out
  '';

  hp = haskellPackages.override {
    overrides = self: super: {
      # hakyll site generator
      lojbanistan-sitegen = super.callPackage "${nixfile}" {};
    };
  };
  bin = "${hp.lojbanistan-sitegen}/bin/lojbanistan-de";

in
stdenv.mkDerivation {
  name = "lojbanistan.de-${version}";

  inherit src;

  buildPhase = ''
    # only english for now
    cp ${jbovlaste}/en.xml ./jbovlaste.xml
    ${bin} build
  '';
  installPhase = ''
    mkdir $out
    echo cp -r ./_site/* $out
  '';
}
