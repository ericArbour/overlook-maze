{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc864"
, isJS ? false }:
let

  pkgs_ = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
  };

  shpadoinkle = builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "92f0e10a93882d193c390df7292685c8e02f8ed3";
    ref    = "docs";
  };

  shpadoinkle-overlay =
    import (shpadoinkle + "/overlay.nix") { inherit compiler isJS; };

  reflex-overlay =
    import (shpadoinkle + "/overlay-reflex.nix") { inherit compiler isJS; };

  haskell-overlay = hself: hsuper:
   { Shpadoinkle                  = hself.callCabal2nix "Shpadoinkle"                  (shpadoinkle + "/core")              {};
     Shpadoinkle-backend-snabbdom = hself.callCabal2nix "Shpadoinkle-backend-snabbdom" (shpadoinkle + "/backends/snabbdom") {};
     Shpadoinkle-backend-static   = hself.callCabal2nix "Shpadoinkle-backend-static"   (shpadoinkle + "/backends/static")   {};
     Shpadoinkle-backend-pardiff  = hself.callCabal2nix "Shpadoinkle-backend-pardiff"  (shpadoinkle + "/backends/pardiff")  {};
     Shpadoinkle-lens             = hself.callCabal2nix "Shpadoinkle-lens"             (shpadoinkle + "/lens")              {};
     Shpadoinkle-html             = hself.callCabal2nix "Shpadoinkle-html"             (shpadoinkle + "/html")              {};
     Shpadoinkle-router           = hself.callCabal2nix "Shpadoinkle-router"           (shpadoinkle + "/router")            {};
     Shpadoinkle-widgets          = hself.callCabal2nix "Shpadoinkle-widgets"          (shpadoinkle + "/widgets")           {};
   };

  snowman-overlay = self: super: {
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
    overlays = [
      shpadoinkle-overlay
      reflex-overlay
      snowman-overlay
    ];
  };

in
  pkgs.haskell.packages.${compiler}.callCabal2nix "snowman" ./. {}

