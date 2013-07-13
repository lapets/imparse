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

toAbstractSyntax :: Parser a -> Compile String ()
toAbstractSyntax p =
  do raw "module AbstractSyntax"
     newline
     raw "  where"
     newlines 2
     toDatatype p
     newline
     raw "--eof"

toDatatype :: Parser a -> Compile String ()
toDatatype (Parser _ _ ps) =
  let production :: Production a -> Compile String ()
      production (Production _ e cs) =
        do raw "data "
           raw e
           raw " = "
           indent
           newline
           raw "  "
           choices $ concat cs
           unindent
           newlines 2

      choices :: [Choice a] -> Compile String ()
      choices cs = case cs of
        [c]  -> 
          do choice c
             newline
             raw "deriving (Show, Eq)"
        c:cs ->
          do choice c 
             newline
             raw "| "
             choices cs

      choice :: Choice a -> Compile String ()
      choice c = case c of
        PrecedenceSeparator -> do nothing
        Choice con assoc es -> 
          do con <-
               case con of
                 Nothing  -> do { c <- fresh; return $ "C" ++ c }
                 Just con -> return con
             raw con
             raw " "
             mapM element es
             nothing
      
      element :: Element a -> Compile String ()
      element e = case e of
        NonTerminal _ entity -> do { raw entity; raw " " }
        Many e _ _           -> do { raw "["; elementNoSp e; raw "] " }
        Indented e           -> element e
        StringLiteral        -> raw "String"
        NaturalLiteral       -> raw "Integer"
        FloatLiteral         -> raw "Double"
        RegExp _             -> raw "String"
        _                    -> do nothing

      elementNoSp :: Element a -> Compile String ()
      elementNoSp e = case e of
        NonTerminal _ entity -> do { raw entity }
        Many e _ _           -> do { raw "["; element e; raw "]" }
        _                    -> element e

  in do mapM production ps
        nothing

toParsec :: Parser a -> Compile String ()
toParsec p = do nothing

--eof
