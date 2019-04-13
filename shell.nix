{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc864.ghcWithPackages (pkgs: [
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
        ]))
        (python37.withPackages(ps: with ps; [
            matplotlib
            pandas
            scikitlearn
        ]))
    ];
    shellHook = ''
        . .shellhook
    '';
}
