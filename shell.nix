{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc864.ghcWithPackages (pkgs: [
            pkgs.cairo
            pkgs.containers
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
            pkgs.random
            pkgs.tf-random
        ]))
    ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
        if [ ! -d bin/ ]; then
            mkdir bin/
        fi
    '';
}
