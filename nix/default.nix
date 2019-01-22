with { fetch = import ./fetch.nix; };
{ nixpkgs ? fetch.nixpkgs }:
import nixpkgs {
  config = { };
  overlays = [
    (self: super:
      { haskellPackages =
          super.haskellPackages.extend
            (super.haskell.lib.packageSourceOverrides
              { inline-rust = fetch.inline-rust;
                t-rex = super.lib.cleanSource ../.;
              }
            );
      }
    )

    (self: super:
      { haskellPackages =
          super.haskellPackages.extend
            (haskellSelf: haskellSuper:
              { inline-rust =
                  let
                    fixMonadFail = pkg:
                      super.haskell.lib.appendConfigureFlag pkg
                        "--ghc-options=-XNoMonadFailDesugaring";
                    rustBuildTools = pkg:
                      super.haskell.lib.addBuildTools pkg
                        [ super.cargo super.rustc ];
                    noHaddock = super.haskell.lib.dontHaddock;
                  in
                    noHaddock (
                      fixMonadFail (
                      rustBuildTools
                      haskellSuper.inline-rust
                      ));
              }
            );
      }
    )

  ];
}
