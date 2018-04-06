
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./phil2.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  drv
