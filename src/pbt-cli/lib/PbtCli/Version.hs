-- | Version and platform identification for the binary.
module PbtCli.Version (
  versionNumber,
  versionLine,
  targetTriple,
) where

import Data.Version (showVersion)
import Paths_convex_pbt_cli (version)
import System.Info (arch, os)

{- | Just the version, e.g. @"0.1.0.0"@. Comes from the @.cabal@ file, so the
released asset name and @--version@ can never drift apart.
-}
versionNumber :: String
versionNumber = showVersion version

{- | The platform this binary was built for, derived from 'System.Info' and
spelled the way the release assets are — so a user can check that they
downloaded the right one.

The released targets are @linux-x64@, @linux-arm64@ and @darwin-arm64@; Intel
macOS is not built for, being considered obsolete in the Cardano ecosystem.
This is computed rather than hardcoded, so a source build on any other
platform still reports itself honestly (an Intel macOS build would say
@darwin-x64@).
-}
targetTriple :: String
targetTriple = os <> "-" <> normalisedArch
 where
  normalisedArch = case arch of
    "x86_64" -> "x64"
    "aarch64" -> "arm64"
    other -> other

-- | The full @--version@ output.
versionLine :: String
versionLine = "pbt-cli " <> versionNumber <> " (" <> targetTriple <> ")"
