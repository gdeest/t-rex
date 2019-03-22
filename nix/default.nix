with { fetch = import ./fetch.nix; };
{ nixpkgs ? fetch.nixpkgs }:
import nixpkgs {
  config = { };
  overlays = [
    (self: super:
      { haskellPackages =
          super.haskellPackages.extend
            (super.haskell.lib.packageSourceOverrides
              { t-rex = super.lib.cleanSource ../.;
              }
            );
      }
    )
  ];
}
