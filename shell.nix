{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
        ]))
        (python37.withPackages(ps: with ps; [
            flake8
            matplotlib
            pandas
            scikitlearn
        ]))
    ];
    shellHook = ''
        . .shellhook
    '';
}
