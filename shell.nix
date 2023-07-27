with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "my-env";
  buildInputs = [
    cabal-install
    haskell.compiler.ghc962
  ];
}
