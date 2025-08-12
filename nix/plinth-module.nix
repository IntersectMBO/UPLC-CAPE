# Plinth/Haskell module for UPLC-CAPE
# This module provides Plinth/PlutusTx tooling and Haskell development support
{
  # Additional flake inputs needed for Plinth/Haskell support
  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    # Plutus and Cardano tooling
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    plutus = {
      url = "github:IntersectMBO/plutus";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # Function to extend a devShell with Plinth/Haskell support
  extendDevShell =
    {
      pkgs,
      baseShell,
      haskell-nix,
      hackage,
      CHaP,
      plutus,
      iohk-nix,
    }:
    let
      # Import nixpkgs with haskell.nix overlays
      haskellPkgs = import pkgs.path {
        inherit (pkgs) system;
        config = haskell-nix.config;
        overlays = [
          iohk-nix.overlays.crypto
          iohk-nix.overlays.cardano-lib
          haskell-nix.overlay
          iohk-nix.overlays.haskell-nix-crypto
          iohk-nix.overlays.haskell-nix-extra
        ];
      };

      # Plinth project configuration
      plinthProject = haskellPkgs.haskell-nix.cabalProject' {
        name = "uplc-cape-benchmarks";
        compiler-nix-name = "ghc966";
        src = ../plinth;
        inputMap = {
          "https://chap.intersectmbo.org/" = CHaP;
        };
        modules = [
          {
            packages = { };
          }
        ];
      };

      # Plinth development tools
      plinthTools = {
        cabal = plinthProject.tool "cabal" "latest";
        haskell-language-server = plinthProject.tool "haskell-language-server" "latest";
      };

      # Additional buildInputs for Plinth/Haskell support
      plinthBuildInputs = with pkgs; [
        # System dependencies for Cardano/Plutus
        pkg-config
        libsodium
        secp256k1
        haskellPkgs.libblst

        # Plinth/PlutusTx tooling
        plinthTools.cabal
        plinthTools.haskell-language-server

        # GHC compiler (matching the project's GHC version)
        haskellPkgs.haskell.compiler.ghc966

        # Haskell-specific formatting tools
        fourmolu # Haskell formatter
        haskellPkgs.haskellPackages.cabal-fmt # Cabal file formatter
      ];
    in
    pkgs.mkShell {
      buildInputs = baseShell.buildInputs ++ plinthBuildInputs;
      inherit (baseShell) shellHook;
    };
}
