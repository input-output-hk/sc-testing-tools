-- | @pbt-cli@ entry point.
module Main (main) where

import Options.Applicative (customExecParser, prefs, showHelpOnEmpty, showHelpOnError)
import PbtCli.Options (commandParserInfo)
import PbtCli.Run (execute)
import System.Exit (exitWith)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  -- Line buffering matters for `pbt-cli stream`: a consumer reading the NDJSON
  -- from a pipe must see each event as it happens, and stdout to a pipe is
  -- block-buffered by default.
  hSetBuffering stdout LineBuffering
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) commandParserInfo
  exitWith =<< execute cmd
