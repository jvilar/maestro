let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          diagrams-gi-gtk = pkgs.haskell.lib.doJailbreak
            (haskellPackagesNew.callPackage ./diagrams-gi-gtk.nix {});
          maestro = haskellPackagesNew.callPackage ./maestro.nix {};
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; }; # pin the channel to ensure reproducibility!
in
  {
    maestro = pkgs.haskellPackages.maestro;
  }
