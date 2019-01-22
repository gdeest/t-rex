let
  pkgs = import ./nix {}; in

{ inherit (pkgs.haskellPackages) inline-rust t-rex; }
