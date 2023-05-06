{ withHoogle ? false, forceShell ? false }:
# A tutorial on Nix, and how you can use 'developPackage' to override
# dependencies:
#   https://www.srid.ca/1948201.html
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/
  nixpkgsRev = "a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31";
  nixpkgsSha = "sha256:162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  # We are using the default compiler (8.8 as of this writing) in nixpkgs.
  # To override, set it to for example: pkgs.haskell.packages.ghc865
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {} ;


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      # compiler.stack
      # compiler.cabal-install
      # compiler.ghcid
      compiler.haskell-language-server
      # compiler.ghcide
      pkgs.ormolu
      # pkgs.hpack
      pkgs.pcre
      # compiler.ghc
      pkgs.postgresql
      pkgs.libpqxx
      pkgs.zlib
      pkgs.haskellPackages.postgresql-libpq
      # pkgs.postgresql-contrib
      pkgs.haskellPackages.record-dot-preprocessor
      # pkgs.redis
    ];

}

# to run HLS 1) delete Setup.hs 2) run 'gen-hie > hie.yaml