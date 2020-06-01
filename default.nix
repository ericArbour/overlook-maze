{ pkgs_ ? <nixpkgs>, compiler ? "ghc865", isJS ? true }:
let
  shpadoinkle = (import pkgs_ {}).fetchgit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "243b89a00c29cdce6768be1b743e846a0bc22fea";
    sha256 = "06v4mgni4nq7asxy96761hgrdzhlva36vzak0l4yxc727zfwrffr";
  };

  shpadoinkle-overlay =
    import (shpadoinkle + "/overlay.nix") { inherit compiler isJS; };

  reflex-overlay =
    import (shpadoinkle + "/overlay-reflex.nix") { inherit compiler isJS; };

  haskell-overlay = hself: hsuper: import shpadoinkle { inherit compiler isJS; };

  my-overlay = self: super: {
    haskell = super.haskell //
      { packages = super.haskell.packages //
        { ${compiler} = super.haskell.packages.${compiler}.override (old: {
            overrides = super.lib.composeExtensions
              (old.overrides or (_: _: {})) haskell-overlay;
          });
        };
      };
    };

  pkgs = import pkgs_ {
    overlays = [ shpadoinkle-overlay reflex-overlay my-overlay ];
  };

in
  pkgs.haskell.packages.${compiler}.callCabal2nix "my-ui" ./. {}

