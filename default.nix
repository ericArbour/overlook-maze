{ chan ? "e1843646b04fb564abf6330a9432a76df3269d2f"
, compiler ? "ghc864"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false
, asShell ? false
}:
let

  shpadoinkle = builtins.fetchGit {
    url    = https://gitlab.com/fresheyeball/Shpadoinkle.git;
    rev    = "72715989495c80dfdf0b5b1894ee2878ff19e7a8";
    ref    = "docs";
  };

  compilerjs = (import (shpadoinkle + "/util.nix") { inherit compiler isJS; }).compilerjs;

  chill = p: (pkgs.haskell.lib.overrideCabal p {
    inherit enableLibraryProfiling enableExecutableProfiling;
  }).overrideAttrs (_: {
    inherit doHoogle doHaddock strictDeps;
  });

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

  if inNixShell || asShell
  then shellFor {
    inherit withHoogle;
    packages = _: [snowman];
    COMPILER = compilerjs;
    buildInputs = [ stylish-haskell cabal-install ghcid lolcat ];
    shellHook = ''
      lolcat ${./figlet}
    '';
  } else chill snowman
