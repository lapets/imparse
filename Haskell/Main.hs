----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Main.hs@
--
--   Main module for the Haskell implementation of the Imparse
--   parser. Compiled into a Haskell executable wrapper for the
--   Imparse parser definition parser.
--

----------------------------------------------------------------
--

module Main
  where

import System.Environment (getArgs)
import System.IO

import Text.Imparse (cmd)

----------------------------------------------------------------
-- | The main function.

main :: IO ()
main =
  do{ args <- getArgs
    ; cmd [] args
    }

--eof
