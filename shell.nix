let
  pkgs = import ./nix {};
in pkgs.haskellPackages.shellFor
  {
    packages = ps: [ ps.t-rex ];
    buildInputs = [ pkgs.cabal-install ];
    withHoogle = false;
  }
