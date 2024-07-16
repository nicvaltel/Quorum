{ nixpkgs ? import <nixpkgs> {  } }:
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/

  nixpkgsRev2 = "249fbde2a178a2ea2638b65b9ecebd531b338cf9";
  nixpkgsSha2 = "0w21qcdk79dmnz0ijbx4mlxbks835nbzfmn0bm1jhx9d0cfj4wr1";

  nixpkgsRev = "ea5234e7073d5f44728c499192544a84244bf35a";
  nixpkgsSha = "sha256:1iqfglh1fdgqxm7n4763k1cipna68sa0cb3azm2gdzhr374avcvk";

  
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {} ;

  pkgs2 = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev2}.tar.gz";
    sha256 = nixpkgsSha2;
  }) {} ;

  


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      compiler.stack
      compiler.cabal-install
      compiler.ghcid
      compiler.haskell-language-server
      compiler.ghcide
      pkgs.ormolu
      pkgs.hpack
      compiler.ghc
      pkgs.haskellPackages.record-dot-preprocessor
      pkgs.zlib

      pkgs.pcre # for pcre-heavy dependency in haskell
      pkgs.postgresql
      pkgs.libpqxx
      pkgs.zlib # sudo apt-get install liblzma-dev
      pkgs.haskellPackages.postgresql-libpq
      pkgs2.haskellPackages.postgresql-simple-migration
      # sudo apt-get install liblzma-dev libghc-postgresql-simple-dev

      # pkgs.redis

    ];

}


# To run HLS:
# stack new MyProgram
# rm Setup.hs 
# rm stack.yaml 
# rm MyProgram.cabal
# rm -rf .stack-work/
# hpack
# gen-hie > hie.yaml
# cabal build