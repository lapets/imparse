----------------------------------------------------------------
--
-- Imparse
-- Cross-platform/-language parser generator.
--
-- Main.hs
--   Haskell executable wrapper for the Imparse parser parser.
--
--
-- * Usage:
--
--   imparse "path/file.p"
--

----------------------------------------------------------------
-- Main module for the Haskell implementation of the
-- Imparse parser.

module Main
  where

import System.Environment (getArgs)
import System.IO

import Text.Imparse (cmd)

----------------------------------------------------------------
-- The main function.

main :: IO ()
main =
  do{ args <- getArgs
    ; cmd [] args
    }

--eof
