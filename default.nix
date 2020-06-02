{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc864"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false }:
let

  pkgs_ = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
  };

  shpadoinkle = if true then ../Shpadoinkle else builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "92f0e10a93882d193c390df7292685c8e02f8ed3";
    ref    = "docs";
  };

  compilerjs = (import (shpadoinkle + "/util.nix") { inherit compiler isJS; }).compilerjs;

  shpadoinkle-overlay =
    import (shpadoinkle + "/overlay.nix") { inherit compiler isJS; };

  reflex-overlay =
    import (shpadoinkle + "/overlay-reflex.nix") { inherit compiler isJS; };

  chill = p: (pkgs.haskell.lib.overrideCabal p {
    inherit enableLibraryProfiling enableExecutableProfiling;
  }).overrideAttrs (_: {
    inherit doHoogle doHaddock strictDeps;
  });

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
        { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
            overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
          });
        };
      };
    };

  pkgs = import
    (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
    }) {
    overlays = [
      shpadoinkle-overlay
      reflex-overlay
      snowman-overlay
    ];
  };

  snowman = pkgs.haskell.packages.${compilerjs}.callCabal2nix "snowman" ./. {};

in with pkgs; with lib; with haskell.packages.${compiler};

  if inNixShell
  then shellFor {
    inherit withHoogle;
    packages = _: [snowman];
    COMPILER = compilerjs;
    buildInputs = [ stylish-haskell cabal-install ghcid ];
    shellHook = ''
      echo "☃️  -> 🥔 ☃️"
    '';
  } else chill snowman
