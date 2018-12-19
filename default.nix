## { compiler ? "ghc822" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =  pkgs.haskellPackages.override {
##          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
             overrides = haskellPackagesNew: haskellPackagesOld: rec {
             # this does not work in 8.4+ because it does not define a semigroup instance
               # data-r-tree =
               #   pkgs.haskell.lib.dontHaddock (
               #     pkgs.haskell.lib.dontCheck (
               #       haskellPackagesNew.callPackage ./data-r-tree.nix { }));

               # myutils =
               #   haskellPackagesNew.callPackage ../../myutils.nix { };

               # day3 =
               #   haskellPackagesNew.callPackage ./day3.nix { };
               lamb = haskellPackagesNew.callPackage ./lamb.nix { };
               };
             };
           };
##         };
       };

  pkgs = import <nixpkgs> { inherit config; };

in
{ lamb = pkgs.haskellPackages.lamb;
}
