with
rec {
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          inline-rust =
            pkgs.haskell.lib.appendConfigureFlags (
              pkgs.haskell.lib.addBuildTools (
                pkgs.haskell.lib.overrideCabal
                  (haskellPackagesNew.callCabal2nix "inline-rust" "${inline-rust-src}" {})
                  (oldDerivation: {
                    doHaddock = false;
                  })
              )
              (with pkgs; [cargo rustc])
            ) [ "--ghc-option=-XNoMonadFailDesugaring" ];
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
  t-rex = haskellPackages.callCabal2nix "t-rex" ./. {};

  haskellPackages = pkgs.haskellPackages;
  inline-rust-src =
    let rev = "5ecff8c92526000e5fc358a2dfede9b60ef59a1a";
	      sha256 = "1ni3bhlj2wv6i17lzihrfj2fqjh6k8ppicrqwmvk6r3r1cjc8gav";
        owner = "harpocrates";
        repo = "inline-rust"; in

    pkgs.fetchzip {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.zip";
      inherit sha256;
    };
};

with pkgs;
with haskellPackages;
  {
    t-rex = t-rex;
    pkgs = pkgs;
    t-rex-shell = haskellPackages.shellFor {
      packages = p: with p; [inline-rust];
      buildInputs =
        [
          cabal-install hlint ghcid
	        rustc
	        cargo
	      ];
    };
  }
