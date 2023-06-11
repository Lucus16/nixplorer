{ ghcVersion ? "ghc92", pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = if (ghcVersion != null) then
    pkgs.haskell.packages.${ghcVersion}
  else
    pkgs.haskellPackages;

in haskellPackages.callCabal2nix "nixplorer" ./. { }
