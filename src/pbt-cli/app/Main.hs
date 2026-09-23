-- | @pbt-cli@ entry point.
module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)
import PbtCli.Options (commandParserInfo)
import PbtCli.Run (classifyIoError, execute)
import System.Exit (ExitCode, exitWith)
import System.IO (BufferMode (LineBuffering), hClose, hFlush, hSetBuffering, hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  -- The output carries a few non-ASCII characters (em dashes in the help text
  -- and the implicit-project label, a middle dot for unrecognised events), and
  -- a handle's encoding otherwise follows the locale: under LC_ALL=C -- the
  -- norm in Docker images and CI runners, which is exactly where this runs --
  -- writing any of them would throw.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Line buffering matters for `run --json`: a consumer reading the NDJSON from
  -- a pipe must see each event as it happens, and stdout to a pipe is
  -- block-buffered by default.
  hSetBuffering stdout LineBuffering
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) commandParserInfo
  code <- execute cmd
  exitWith =<< flushing code

{- | Flush stdout before exiting, so a write failure carries our exit code
rather than the runtime's.

Buffered output is only written when the handle is flushed, and if that first
happens during the runtime's own shutdown then the exception is not ours to
classify: it reaches GHC's top-level handler, which prints it and exits 1,
overriding whatever 'execute' decided. Flushing here — and closing the handle
when the flush fails, so the runtime does not retry it — keeps the decision.
-}
flushing :: ExitCode -> IO ExitCode
flushing code = do
  flushed <- try (hFlush stdout)
  case flushed of
    Right () -> pure code
    Left (e :: IOException) -> do
      void (try (hClose stdout) :: IO (Either IOException ()))
      pure (classifyIoError e)
