
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./phil2.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  unification = haskellPackages.callPackage ./nix/unification.nix {};
  drv = haskellPackages.callPackage f {
    inherit unification;
    containers = haskellPackages.containers_0_5_10_2;
    };

in

  drv
