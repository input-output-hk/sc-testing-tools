{ inputs, pkgs, lib }:

let
  # Coverage annotations for the PlutusTx scripts, so that the nix test
  # derivations ($PROJECT#$PACKAGE:test:$SUITE) always report coverage without
  # anyone having to remember a build argument. Kept out of the .cabal files so
  # that a plain `cabal test` stays switchable from the command line -- see
  # "Turning coverage on and off" in the README.
  #
  # Only suites that survive instrumentation are listed below. The auction,
  # escrow and vesting suites are left out: coverage grows their transactions
  # past the 16384 byte max tx size and they have no limit adaptation, so they
  # fail outright. Add them here once their Spec picks transaction limits the
  # way MultiPlayerPingPong's does.
  coverageOptions = [ "-g" "-fplugin-opt" "PlutusTx.Plugin:coverage-all" ];

  cabalProject = pkgs.haskell-nix.cabalProject' (

    { config, pkgs, ... }:

    {
      name = "sc-tools";

      compiler-nix-name = lib.mkDefault "ghc966";

      src = lib.cleanSource ../.;

      flake.variants = {
        ghc966 = { }; # Alias for the default variant
        #ghc984.compiler-nix-name = "ghc984";
        #ghc9102.compiler-nix-name = "ghc9102";
        #ghc9122.compiler-nix-name = "ghc9122";
      };

      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

      cabalProjectLocal = ''
        package *
          ghc-options=-Werror
      '';
      modules = [{
        packages = {
          convex-testing-interface.ghcOptions = [ "-Werror" ];

          convex-tasty-streaming.components.tests.convex-tasty-streaming-test.ghcOptions = coverageOptions;
          convex-testing-interface.components.tests.convex-testing-interface-test.ghcOptions = coverageOptions;
          convex-use-cases.components.tests = {
            convex-multiplayerpingpong-test.ghcOptions = coverageOptions;
            convex-rewardwithdrawal-test.ghcOptions = coverageOptions;
          };
        };
      }];
    }
  );

in

cabalProject

