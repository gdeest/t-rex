let
  pkgs = import ./nix {};
in pkgs.haskellPackages.shellFor
  {
    packages = ps: [ ps.t-rex ];
    buildInputs = with pkgs; [ cabal-install llvm_6 haskellPackages.ghcid ];
    withHoogle = false;
  }
