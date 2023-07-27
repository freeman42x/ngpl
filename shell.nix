let
  unstable = import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz") {};
  pkgs = import <nixpkgs> {};
in
pkgs.stdenv.mkDerivation {
  name = "my-env";
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell.compiler.ghc962
    unstable.haskellPackages.haskell-language-server
  ];
}
