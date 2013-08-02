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

module Text.Imparse.Compile.Haskell
  where

import Data.Char (toLower)
import Data.String.Utils (join, replace)
import Data.Maybe (catMaybes)
import Control.Compilation.Compile

import Text.Imparse.AbstractSyntax
import qualified Text.Imparse.Analysis as S

----------------------------------------------------------------
-- Helper functions.

toLowerFirst :: String -> String
toLowerFirst []     = []
toLowerFirst (c:cs) = toLower c : cs

----------------------------------------------------------------
-- Compilation to abstract syntax data type definition.

toAbstractSyntax :: String -> Parser a -> Compile String ()
toAbstractSyntax prefix p =
  do prefix <- return $ if prefix == "" then "" else prefix ++ "."
     raw $ "-- This module generated automatically by imparse.\n\n"
     raw $ "module " ++ prefix ++ "AbstractSyntax\n"
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
           choices $ concat [cs | Choices _ cs <- css]
           raw "deriving (Show, Eq)"
           unindent
           newlines 2

      choices :: [Choice a] -> Compile String ()
      choices cs = case cs of
        [c]  -> 
          do choice c
             newline
        c:cs ->
          do choice c 
             newline
             raw "| "
             choices cs

      choice :: Choice a -> Compile String ()
      choice c = case c of
        Choice _ con _ es -> 
          do con <-
               case con of
                 Nothing  -> do { c <- fresh; return $ "C" ++ c }
                 Just con -> return con
             raw con
             mapM element es
             nothing

      element :: Element a -> Compile String ()
      element e = case e of
        NonTerminal _ entity -> do { raw " "; raw entity }
        Many e _ _           -> do { raw " ["; elementNoSp e; raw "]" }
        Indented e           -> element e
        Terminal t           -> do { raw " "; terminal t }
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
  do raw $ "-- This module generated automatically by imparse.\n\n"
     prefix <- return $ if prefix == "" then "" else prefix ++ "."
     raw $ "module " ++ prefix ++ "Report"
     newline
     raw "  where"
     newlines 2
     raw "import qualified Text.RichReports as R"
     newlines 2
     raw $ "import " ++ prefix ++ "AbstractSyntax"
     newlines 2
     toReportFuns p
     newline
     raw "--eof"

toReportFuns :: Parser a -> Compile String ()
toReportFuns (Parser _ _ ps) =
  let production :: Production a -> Compile String ()
      production (Production _ e css) =
        do raw $ "instance R.ToReport " ++ e ++ " where"
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
        NonTerminal _ entity   -> Just $ "R.report " ++ v
        Many e' _ _            -> element (v,e')
        Indented (Many e' _ _) -> maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [R.Line [] [R.report vx] | vx <- " ++ v ++ "]") $ element (v,e')
        Indented e'            -> maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [" ++ r ++ "]") $ element (v,e')
        Terminal t             -> Just $ terminal v t
        _                      -> Nothing

      terminal :: String -> Terminal -> String
      terminal v t = case t of
        Explicit s     -> "R.key \"" ++ s ++ "\""
        NewLine        -> "R.Line [] []"
        StringLiteral  -> "R.lit " ++ v
        NaturalLiteral -> "R.lit (show " ++ v ++ ")"
        DecimalLiteral -> "R.lit " ++ v
        Identifier     -> "R.var " ++ v
        Constructor    -> "R.Text " ++ v
        Flag           -> "R.Text " ++ v
        RegExp _       -> "R.Text " ++ v

  in do mapM production ps
        nothing

----------------------------------------------------------------
-- Compilation to Parsec parser.

toParsec :: String -> Parser S.Analysis -> Compile String ()
toParsec prefix (p@(Parser _ _ ((Production _ eRoot _):_))) =
  do raw $ "-- This module generated automatically by imparse.\n\n"
     prefix <- return $ if prefix == "" then "" else prefix ++ "."
     raw $ "module " ++ prefix ++ "Parse\n  where\n"
     newline
     raw $ "import " ++ prefix ++ "AbstractSyntax"
     newlines 2

     -- template <- return $ replace "\n\n" "\n" $ replace "\r" "" $ unpack $(embedFile "Text/Imparse/Compile/parsec.template")

     raw "----------------------------------------------------------------\n-- Parser to convert concrete syntax to abstract syntax.\n\n"
     raw "import Text.Parsec\n"
     raw "import qualified Text.Parsec.Indent as PI (withBlock, runIndent)\n"
     raw "import qualified Text.Parsec.Token as PT\n"
     raw "import qualified Text.Parsec.Expr as PE\n"
     raw "import qualified Text.ParserCombinators.Parsec.Language as PL\n"
     raw "import qualified Text.ParserCombinators.Parsec.Prim as Prim\n\n"
     raw "import Control.Monad.Trans.State.Lazy (StateT)\n"
     raw "import Data.Functor.Identity (Identity)\n\n"
     raw "----------------------------------------------------------------\n-- Parsing functions to export.\n\n"
     raw $ "parseString :: String -> Either ParseError " ++ eRoot ++ "\n"
     raw "parseString s = PI.runIndent \"\" $ runParserT root () \"\" s\n\n"
     raw "----------------------------------------------------------------\n-- Parser state.\n\n"
     raw "type ParseState = StateT SourcePos Identity\n"
     raw "type ParseFor a = ParsecT [Char] () ParseState a\n\n"
     raw "----------------------------------------------------------------\n-- Parsec-specific configuration definitions and synonyms.\n\n"
     raw "langDef :: PL.GenLanguageDef String () ParseState\n"
     raw "langDef = PL.javaStyle\n"
     raw "  { PL.identStart        = oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijkmlnopqrstuvwxyz_\" -- Only lowercase.\n"
     raw "  , PL.identLetter       = alphaNum <|> oneOf \"_'\"\n"
     raw "  , PL.opStart           = PL.opLetter langDef\n"
     raw "  , PL.opLetter          = oneOf \"\"\n"
     raw "  , PL.reservedOpNames   = [ ]\n"
     raw "  , PL.reservedNames     = [ ]\n"
     raw "  , PL.commentLine       = \"#\"\n"
     raw "  }\n\n"
     raw "lang :: PT.GenTokenParser [Char] () ParseState\n"
     raw "lang = PT.makeTokenParser langDef\n\n"
     raw "whiteSpace = PT.whiteSpace lang\n"
     raw "symbol     = PT.symbol lang\n"
     raw "rO         = PT.reservedOp lang\n"
     raw "res        = PT.reserved lang\n"
     raw "identifier = PT.identifier lang\n"
     raw "natural    = PT.natural lang\n\n"
     raw "binary name f assoc = PE.Infix (do{PT.reservedOp lang name; return f}) assoc\n"
     raw "prefix name f       = PE.Prefix (do{PT.reservedOp lang name; return f})\n\n"
     raw "withIndent p1 p2 f = PI.withBlock f p1 p2\n\n"
     raw "con :: ParseFor String\n"
     raw "con = do { c <- oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\" ; cs <- option \"\" identifier ; return $ c:cs }\n\n"
     raw "flag :: ParseFor String\n"
     raw "flag = do { cs <- many1 (oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") ; return cs }\n"
     raw "-- caps = do { cs <- many1 (oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") ; return cs }\n\n"
     raw "(<?|>) p1 p2 = (try p1) <|> p2\n\n"
     raw "----------------------------------------------------------------\n-- Parser definition.\n\n"
     raw $ "root = do { whiteSpace ; r <- p" ++ eRoot ++ " ; eof ; return r }"
     newlines 2
     toParsecDefs p
     raw "--eof"

toParsecDefs :: Parser S.Analysis -> Compile String ()
toParsecDefs (Parser _ _ ps) =
  let production :: Production S.Analysis -> Compile String ()
      production (p@(Production _ e css)) =
        do raw $ "p" ++ e ++ " ="
           ( if S.ProductionInfixPrefixThenDeterministic `elem` S.tags p then
               do ops <- return $ join "," $
                           [     "[" 
                              ++ join "," 
                                   [ case es of
                                       [Terminal (Explicit op), _]    -> "prefix \"" ++ op ++ "\" " ++ con ++ ""
                                       [_, Terminal (Explicit op), _] -> "binary \"" ++ op ++ "\" " ++ con ++ " PE.AssocLeft"
                                   | Choice _ (Just con) asc es <- cs
                                   ]
                              ++ "]"
                           | Choices _ cs <- init css
                           ]
               
                  raw $ " PE.buildExpressionParser [" ++ ops ++ "] ("
                  indent
                  ( if ((length css > 1) || (or [length cs > 1 | Choices _ cs <- css])) then
                      do { newline ; raw "    " }
                    else
                      raw " "
                    )
                  choices $ last [cs | Choices _ cs <- css]
                  raw ")"
                  unindent
                  newline
             else
               do indent
                  ( if ((length css > 1) || (or [length cs > 1 | Choices _ cs <- css])) then
                      do { newline ; raw "    " }
                    else
                      raw " "
                    )
                  choices $ concat [cs | Choices _ cs <- css]
                  unindent
                  newline
             )

      choices :: [Choice S.Analysis] -> Compile String ()
      choices cs = case cs of
        [c]  -> 
          do choice c
             newline
             raw ""
        c:cs ->
          do choice c 
             newline
             raw "<|> "
             choices cs

      choice :: Choice S.Analysis -> Compile String ()
      choice (c@(Choice _ con _ es)) =
        if S.ChoiceIndentedSuffix `elem` S.tags c then
          do ves <- return $ init [("v" ++ show k, es!!k) | k <- [0..length es-1]]
             con <-
                case con of
                   Nothing  -> do { c <- fresh; return $ "C" ++ c }
                   Just con -> return con
             nt <- return $ (\(Indented (Many (NonTerminal _ nt) _ _)) -> nt) $ last es
             raw $ "withIndent ("
             raw "do {"
             raw $ join "; " (map element ves)
             raw "; "
             raw $ "return $ (" ++ join ", " (catMaybes (map arg ves)) ++ ")"
             raw "}) "
             raw $ "p" ++ nt
             raw $ " (\\(" ++ (join ", " $ catMaybes (map arg ves)) ++ ") vs -> " ++ con ++ " " ++ join " " (catMaybes (map arg ves)) ++ " vs)"
        else
          do ves <- return $ [("v" ++ show k, es!!k) | k <- [0..length es-1]]
             con <-
                case con of
                   Nothing  -> do { c <- fresh; return $ "C" ++ c }
                   Just con -> return con
             raw "do {"
             raw $ join "; " (map element ves)
             raw "; "
             raw $ "return $ " ++ con ++ " " ++ join " " (catMaybes (map arg ves))
             raw "}"

      element :: (String, Element S.Analysis) -> String
      element (v, e) = case e of
        NonTerminal _ nt            -> v ++ " <- p" ++ nt
        Many (NonTerminal _ nt) n sep ->
          let cmbSuffix = if n == 0 then "" else "1"
              comb = maybe "many" (\_ -> "sepBy") sep
              suffix = maybe "" (\sep -> " (res \" ++ sep ++ \")") sep
          in v ++ " <- " ++ comb ++ cmbSuffix ++ " p" ++ nt ++ suffix
        Indented e'                 -> ""
        Terminal t                  -> terminal v t
        _                           -> ""

      arg :: (String, Element S.Analysis) -> Maybe String
      arg (v, e) = case e of
        NonTerminal _ nt -> Just v
        Many e' _ _      -> Just v
        Indented e'      -> Just v
        Terminal t       -> argT v t
        _                -> Nothing

      argT :: String -> Terminal -> Maybe String
      argT v t = case t of
        Explicit s     -> Nothing
        NewLine        -> Nothing
        StringLiteral  -> Just v
        NaturalLiteral -> Just v
        DecimalLiteral -> Just v
        Identifier     -> Just v
        Constructor    -> Just v
        Flag           -> Just v
        RegExp r       -> Nothing

      terminal :: String -> Terminal -> String
      terminal v t = case t of
        Explicit s     -> "res \"" ++ s ++ "\""
        NewLine        -> "whiteSpace"
        StringLiteral  -> v ++ " <- literal"
        NaturalLiteral -> v ++ " <- natural"
        DecimalLiteral -> v ++ " <- decimal"
        Identifier     -> v ++ " <- identifier"
        Constructor    -> v ++ " <- con"
        Flag           -> v ++ " <- flag"
        RegExp r       -> "regexp"

  in do mapM production ps
        nothing

--eof
