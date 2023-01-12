{ ghcVersion ? "ghc92", pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = if (ghcVersion != null) then
    pkgs.haskell.packages.${ghcVersion}
  else
    pkgs.haskellPackages;

  ghc = haskellPackages.ghcWithHoogle (h:
    with h; [
      brick
      containers
      directory
      extra
      filepath
      lens
      megaparsec
      mtl
      parser-combinators
      process
      text
      utf8-string
      vty
    ]);

in pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    ghc
    haskellPackages.haskell-language-server
    hlint
    stylish-haskell
  ];
}
