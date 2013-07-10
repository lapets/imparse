----------------------------------------------------------------
--
-- Imparse
--
-- Text/Imparse/ToHaskell.hs
--   Compilation from an Imparse parser definition to a Haskell
--   implementation of a abstract syntax data type and Parsec
--   parser.
--

----------------------------------------------------------------
-- 

module Text.Imparse.ToHaskell
  where

import Control.Compilation.Compile

import Text.Imparse.AbstractSyntax

----------------------------------------------------------------
-- Transformation functions.


toDatatype :: Parser a -> Compile String ()
toDatatype p = do nothing

toParsec :: Parser a -> Compile String ()
toParsec p = do nothing

--eof
