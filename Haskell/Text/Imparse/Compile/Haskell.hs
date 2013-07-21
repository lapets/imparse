----------------------------------------------------------------
--
-- Imparse
--
-- Text/Imparse/Compile/Haskell.hs
--   Compilation from an Imparse parser definition to a Haskell
--   implementation of a abstract syntax data type and Parsec
--   parser.
--

----------------------------------------------------------------
-- 

{-# LANGUAGE TemplateHaskell #-}

module Text.Imparse.Compile.Haskell
  where

import Data.Char (toLower)
import Data.String.Utils (join, replace)
import Data.Maybe (catMaybes)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import Control.Compilation.Compile

import Text.Imparse.AbstractSyntax

----------------------------------------------------------------
-- Helper functions.

toLowerFirst :: String -> String
toLowerFirst []     = []
toLowerFirst (c:cs) = toLower c : cs

----------------------------------------------------------------
-- Compilation to abstract syntax data type definition.

toAbstractSyntax :: String -> Parser a -> Compile String ()
toAbstractSyntax prefix p =
  do raw $ "module " ++ prefix ++ ".AbstractSyntax\n"
     raw "  where"
     newlines 2
     toDatatype p
     newline
     raw "--eof"

toDatatype :: Parser a -> Compile String ()
toDatatype (Parser _ _ ps) =
  let production :: Production a -> Compile String ()
      production (Production _ e css) =
        do raw "data "
           raw e
           raw " = "
           indent
           newline
           raw "  "
           mapM choices css
           unindent
           newlines 2

      choices :: Choices a -> Compile String ()
      choices (Choices a cs) = case cs of
        [c]  -> 
          do choice c
             newline
             raw "deriving (Show, Eq)"
        c:cs ->
          do choice c 
             newline
             raw "| "
             choices (Choices a cs)

      choice :: Choice a -> Compile String ()
      choice c = case c of
        Choice _ con _ es -> 
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
        Terminal t           -> terminal t
        _                    -> do nothing

      elementNoSp :: Element a -> Compile String ()
      elementNoSp e = case e of
        NonTerminal _ entity -> do { raw entity }
        Many e _ _           -> do { raw "["; element e; raw "]" }
        _                    -> element e

      terminal :: Terminal -> Compile String ()
      terminal t = case t of
        StringLiteral  -> raw "String"
        NaturalLiteral -> raw "Integer"
        DecimalLiteral -> raw "Double"
        Identifier     -> raw "String"
        Constructor    -> raw "String"
        Flag           -> raw "String"
        RegExp _       -> raw "String"
        _              -> do nothing        

  in do mapM production ps
        nothing

----------------------------------------------------------------
-- Compilation to rich reporting instance declarations.

toRichReport :: String -> Parser a -> Compile String ()
toRichReport prefix p =
  do raw $ "module " ++ prefix ++ ".Report"
     newline
     raw "  where"
     newlines 2
     raw "import qualified Text.RichReports as R"
     newlines 2
     toReportFuns p
     newline
     raw "--eof"

toReportFuns :: Parser a -> Compile String ()
toReportFuns (Parser _ _ ps) =
  let production :: Production a -> Compile String ()
      production (Production _ e css) =
        do raw $ "instance Report " ++ e ++ " where"
           indent
           newline
           raw "report x = case x of"
           indent
           newline
           mapM choices css
           unindent
           unindent
           newline

      choices :: Choices a -> Compile String ()
      choices (Choices a cs) = case cs of
        []   -> do nothing
        c:cs -> do { choice c; newline; choices (Choices a cs) }

      choice :: Choice a -> Compile String ()
      choice c = case c of
        Choice _ con _ es -> 
          do con <-
               case con of
                 Nothing  -> do { c <- fresh; return $ "C" ++ c }
                 Just con -> return con
             
             ves <- return $ [("v" ++ show k, es!!k) | k <- [0..length es-1]]
             raw $ con ++ " " ++ join " " [v | (v,e) <- ves, isData e] ++ " -> "
             raw $ "R.Span [] [] $ [" ++ join ", " (catMaybes $ map element ves) ++ "]"

      element :: (String, Element a) -> Maybe String
      element (v,e) = case e of
        NonTerminal _ entity -> Just $ "R.report " ++ v
        Many e' _ _          -> element (v,e')
        Indented e'          -> maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [" ++ r ++ "]") $ element (v,e')
        Terminal t           -> Just $ terminal v t
        _                    -> Nothing

      terminal :: String -> Terminal -> String
      terminal v t = case t of
        Explicit s     -> "R.key \"" ++ s ++ "\""
        NewLine        -> "R.Line [] []"
        StringLiteral  -> "R.lit " ++ v
        NaturalLiteral -> "R.lit " ++ v
        DecimalLiteral -> "R.lit " ++ v
        Identifier     -> "R.var " ++ v
        Constructor    -> "R.Text " ++ v
        Flag           -> "R.Text " ++ v
        RegExp _       -> "R.Text " ++ v        

  in do mapM production ps
        nothing

----------------------------------------------------------------
-- Compilation to Parsec parser.

toParsec :: String -> Parser a -> Compile String ()
toParsec prefix p =
  do raw $ "module " ++ prefix ++ ".Parse\n  where\n"
     newline
     raw $ "import " ++ prefix ++ ".AbstractSyntax\n"
     newlines 2
     template <- return $ 
                   replace "\n\n" "\n" $ replace "\r" "" $ 
                   unpack $(embedFile "Text/Imparse/Compile/parsec.template")


     raw template
     newlines 2

     newline
     raw "--eof"

toParsecDefs :: Parser a -> Compile String ()
toParsecDefs (Parser _ _ ps) =
  let production :: Production a -> Compile String ()
      production (Production _ e css) =
        do raw $ "instance Report " ++ e ++ " where"
           indent
           newline
           raw "report x = case x of"
           indent
           newline
           mapM choices css
           unindent
           unindent
           newline

      choices :: Choices a -> Compile String ()
      choices (Choices a cs) = case cs of
        []   -> do nothing
        c:cs -> do { choice c; newline; choices (Choices a cs) }

      choice :: Choice a -> Compile String ()
      choice c = case c of
        Choice _ con _ es -> 
          do con <-
               case con of
                 Nothing  -> do { c <- fresh; return $ "C" ++ c }
                 Just con -> return con
             
             ves <- return $ [("v" ++ show k, es!!k) | k <- [0..length es-1]]
             raw $ con ++ " " ++ join " " [v | (v,e) <- ves, isData e] ++ " -> "
             raw $ "R.Span [] [] $ [" ++ join ", " (catMaybes $ map element ves) ++ "]"
      
      element :: (String, Element a) -> Maybe String
      element (v,e) = case e of
        NonTerminal _ entity -> Just $ "R.report " ++ v
        Many e' _ _          -> element (v,e')
        Indented e'          -> maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [" ++ r ++ "]") $ element (v,e')
        Terminal t           -> Just $ terminal v t
        _                    -> Nothing

      terminal :: String -> Terminal -> String
      terminal v t = case t of
        Explicit s     -> "R.key \"" ++ s ++ "\""
        NewLine        -> "R.Line [] []"
        StringLiteral  -> "R.lit " ++ v
        NaturalLiteral -> "R.lit " ++ v
        DecimalLiteral -> "R.lit " ++ v
        Identifier     -> "R.var " ++ v
        Constructor    -> "R.Text " ++ v
        Flag           -> "R.Text " ++ v
        RegExp _       -> "R.Text " ++ v

  in do mapM production ps
        nothing

--eof
