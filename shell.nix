# $ nix-env --query --available --attr nixpkgs.ghc

with import <nixpkgs> {};
mkShell {
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.hindent
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
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
