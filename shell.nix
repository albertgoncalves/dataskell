{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc864.ghcWithPackages (pkgs: [
            pkgs.hlint
            pkgs.hoogle
            pkgs.HUnit
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
