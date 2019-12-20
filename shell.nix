{ nixpkgs ? import <upkg> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, filepath, directory, hpack, stdenv, vlc, cabal-install }:
      mkDerivation {
        pname = "CueScript";
        version = "0.1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [ base containers filepath directory ];
        librarySystemDepends = [ vlc ];
        libraryToolDepends = [ hpack cabal-install ];
        executableHaskellDepends = [ base containers ];
        doHaddock = false;
        prePatch = "hpack";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
