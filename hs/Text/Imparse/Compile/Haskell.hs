----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Text\/Imparse\/Compile\/Haskell.hs@
--
--   Compilation from an Imparse parser definition to a Haskell
--   implementation of a abstract syntax data type and Parsec
--   parser.
--

----------------------------------------------------------------
-- 

module Text.Imparse.Compile.Haskell
  where

import Data.Char (isAlphaNum, toLower)
import Data.List (nub, (\\))
import Data.String.Utils (join, replace)
import Data.Maybe (catMaybes)
import Control.Compilation (Compile, StateExtension(..), nothing)
import Control.Compilation.String
import Control.Compilation.Fresh

import Text.Imparse.AbstractSyntax
import qualified Text.Imparse.Analysis as S

----------------------------------------------------------------
-- | State data structure

data State =
  State StateExtensionFresh StateExtensionString

instance StateExtension State where
  initial = State initial initial

instance HasFresh State where
  project (State i s) = i
  inject i (State _ s) = State i s

instance HasString State where
  project (State i s) = s
  inject s (State i _) = State i s

----------------------------------------------------------------
-- | Helper functions.

toLowerFirst :: String -> String
toLowerFirst []     = []
toLowerFirst (c:cs) = toLower c : cs

----------------------------------------------------------------
-- | Compilation to abstract syntax data type definition.

toAbstractSyntax :: String -> Parser a -> Compile State ()
toAbstractSyntax prefix p =
  do prefix <- return $ if prefix == "" then "" else prefix ++ "."
     raw $ "-- This module was generated automatically by imparse.\n\n"
     raw $ "module " ++ prefix ++ "AbstractSyntax\n"
     raw "  where"
     newlines 2
     toDatatype p
     newline
     raw "--eof"

toDatatype :: Parser a -> Compile State ()
toDatatype (Parser _ _ ps) =
  let production :: Production a -> Compile State ()
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

      choices :: [Choice a] -> Compile State ()
      choices cs = case cs of
        [c]  -> 
          do choice c
             newline
        c:cs ->
          do choice c 
             newline
             raw "| "
             choices cs

      choice :: Choice a -> Compile State ()
      choice c = case c of
        Choice _ con _ es -> 
          do con <-
               case con of
                 Nothing  -> do { c <- fresh; return $ "C" ++ c }
                 Just con -> return con
             raw con
             mapM element es
             nothing

      element :: Element a -> Compile State ()
      element e = case e of
        NonTerminal _ entity -> do { raw " "; raw entity }
        Many e _             -> do { raw " ["; elementNoSp e; raw "]" }
        May (Many e _)       -> do { raw " ["; elementNoSp e; raw "]" }
        May e                -> do { raw " (Maybe "; elementNoSp e; raw ")" }
        Indented w e         -> element e
        Terminal t           -> do { raw " "; terminal t }
        _                    -> do nothing

      elementNoSp :: Element a -> Compile State ()
      elementNoSp e = case e of
        NonTerminal _ entity -> do { raw entity }
        Many e  _            -> do { raw "["; element e; raw "]" }
        May (Many e _)       -> do { raw "["; element e; raw "]" }
        May e                -> do { raw "(Maybe "; elementNoSp e; raw ")" }
        _                    -> element e

      terminal :: Terminal -> Compile State ()
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
-- | Compilation to rich reporting instance declarations.

toRichReport :: String -> Parser a -> Compile State ()
toRichReport prefix p =
  do raw $ "-- This module was generated automatically by imparse.\n\n"
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

toReportFuns :: Parser a -> Compile State ()
toReportFuns (Parser _ _ ps) =
  let production :: Production a -> Compile State ()
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

      choices :: Choices a -> Compile State ()
      choices (Choices a cs) = case cs of
        []   -> do nothing
        c:cs -> do { choice c; newline; choices (Choices a cs) }

      choice :: Choice a -> Compile State ()
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
        NonTerminal _ entity         -> Just $ "R.report " ++ v
        Many e' _                    -> element (v,e')
        May e'                       -> element (v,e')
        Indented w (May (Many e' _)) -> 
          maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [R.Line [] [R.report vx] | vx <- " ++ v ++ "]") $ element (v,e')
        Indented w (Many e' _)       ->
          maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [R.Line [] [R.report vx] | vx <- " ++ v ++ "]") $ element (v,e')
        Indented w e'                -> 
          maybe Nothing (\r -> Just $ "R.BlockIndent [] [] $ [" ++ r ++ "]") $ element (v,e')
        Terminal t                   -> Just $ terminal v t
        _                            -> Nothing

      terminal :: String -> Terminal -> String
      terminal v t = case t of
        Explicit s     -> "R.key \"" ++ s ++ "\""
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
-- | Compilation to Parsec parser.

toParsec :: String -> Parser S.Analysis -> Compile State ()
toParsec prefix (p@(Parser _ _ ((Production _ eRoot _):_))) =
  do raw $ "-- This module was generated automatically by imparse.\n\n"
     prefix <- return $ if prefix == "" then "" else prefix ++ "."
     raw $ "module " ++ prefix ++ "Parse\n  where\n"
     newline
     raw $ "import " ++ prefix ++ "AbstractSyntax"
     newlines 2

     reservedOpNames <- return $ nub $ S.allOps p
     opLetters <- return $ nub $ [c | c <- concat reservedOpNames, not $ isAlphaNum c]
     reservedNames <- return $ (nub [r | Explicit r <- terminals p]) \\ reservedOpNames

     raw "----------------------------------------------------------------\n-- Parser to convert concrete syntax to abstract syntax.\n\n"
     raw "import Text.Parsec\n"
     raw "import qualified Text.Parsec.Indent as PI (runIndent, checkIndent, withPos, indented, block)\n"
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
     raw $ "  { PL.identStart        = oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijkmlnopqrstuvwxyz_\" -- Only lowercase.\n"
     raw $ "  , PL.identLetter       = alphaNum <|> oneOf \"_'\"\n"
     raw $ "  , PL.opStart           = PL.opLetter langDef\n"
     raw $ "  , PL.opLetter          = oneOf \"" ++ opLetters ++ "\"\n"
     raw $ "  , PL.reservedOpNames   = [" ++ join "," ["\"" ++ rO ++ "\"" | rO <- reservedOpNames] ++ "]\n"
     raw $ "  , PL.reservedNames     = [" ++ join "," ["\"" ++ rO ++ "\"" | rO <- reservedNames] ++ "]\n"
     raw $ "  , PL.commentLine       = \"#\"\n"
     raw "  }"
     newlines 2
     raw "lang :: PT.GenTokenParser [Char] () ParseState\n"
     raw "lang = PT.makeTokenParser langDef"
     newlines 2
     raw "whiteSpace = PT.whiteSpace lang\n"
     raw "symbol     = PT.symbol lang\n"
     raw "rO         = PT.reservedOp lang\n"
     raw "res        = PT.reserved lang\n"
     raw "identifier = PT.identifier lang\n"
     raw "natural    = PT.natural lang"
     newlines 2
     raw "binary name f assoc = PE.Infix (do{PT.reservedOp lang name; return f}) assoc\n"
     raw "prefix name f       = PE.Prefix (do{PT.reservedOp lang name; return f})"
     newlines 2
     raw "con :: ParseFor String\n"
     raw "con = do { c <- oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\" ; cs <- option \"\" identifier ; return $ c:cs }"
     newlines 2
     raw "flag :: ParseFor String\n"
     raw "flag = do { cs <- many1 (oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") ; return cs }\n"
     raw "-- caps = do { cs <- many1 (oneOf \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\") ; return cs }"
     newlines 2
     raw "block0 p = PI.withPos $ do { r <- many (PI.checkIndent >> p); return r }\n"
     raw "may p = option Nothing (do {x <- p; return $ Just x})\n"
     raw "(<?|>) p1 p2 = (try p1) <|> p2\n\n"
     raw "----------------------------------------------------------------\n-- Parser definition.\n\n"
     raw $ "root = do { whiteSpace ; r <- p" ++ eRoot ++ " ; eof ; return r }"
     newlines 2
     toParsecDefs p
     raw "--eof"

toParsecDefs :: Parser S.Analysis -> Compile State ()
toParsecDefs (Parser _ _ ps) =
  let explicitCmb :: String -> String
      explicitCmb s = if isOp s then "rO" else "res" 

      production :: Production S.Analysis -> Compile State ()
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
                      do { newline ; raw "     " }
                    else
                      raw " "
                    )
                  choices $ concat [cs | Choices _ cs <- css]
                  unindent
                  newline
             )

      choices :: [Choice S.Analysis] -> Compile State ()
      choices cs = case cs of
        [c]  -> 
          do choice c
             newline
             raw ""
        c:cs ->
          do choice c 
             newline
             raw "<?|> "
             choices cs

      choice :: Choice S.Analysis -> Compile State ()
      choice (c@(Choice _ con _ es)) =
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
      element (v, e) = 
        let mkP e = case e of
              NonTerminal _ nt         -> "p" ++ nt
              Many e' Nothing          -> "(many1 (" ++ mkP e' ++ "))"
              Many e' (Just sep)       -> "(sepBy1 " ++ mkP e' ++ " (" ++ explicitCmb sep ++ " \"" ++ sep ++ "\"))"
              May (Many e' Nothing)    -> "(many (" ++ mkP e' ++ "))"
              May (Many e' (Just sep)) -> "(sepBy " ++ mkP e' ++ " (" ++ explicitCmb sep ++ " \"" ++ sep ++ "\"))"
              May e'                   -> "(may (" ++ mkP e' ++ "))"
              Indented False e'        -> mkP e'
              Indented True e' ->
                case e' of
                  Many e' Nothing       -> "(PI.indented >> PI.block (" ++ mkP e' ++ "))"
                  May (Many e' Nothing) -> "(PI.indented >> block0 (" ++ mkP e' ++ "))"
              _                        -> ""
        in case e of
          Terminal t -> terminal v t
          _ -> case mkP e of "" -> "" ; p -> v ++ " <- " ++ p

      arg :: (String, Element S.Analysis) -> Maybe String
      arg (v, e) = case e of
        NonTerminal _ nt -> Just v
        Many e' _        -> Just v
        May e'           -> Just v
        Indented _ e'    -> Just v
        Terminal t       -> argT v t
        _                -> Nothing

      argT :: String -> Terminal -> Maybe String
      argT v t = case t of
        Explicit s     -> Nothing
        StringLiteral  -> Just v
        NaturalLiteral -> Just v
        DecimalLiteral -> Just v
        Identifier     -> Just v
        Constructor    -> Just v
        Flag           -> Just v
        RegExp r       -> Nothing

      terminal :: String -> Terminal -> String
      terminal v t = case t of
        StringLiteral  -> v ++ " <- literal"
        NaturalLiteral -> v ++ " <- natural"
        DecimalLiteral -> v ++ " <- decimal"
        Identifier     -> v ++ " <- identifier"
        Constructor    -> v ++ " <- con"
        Flag           -> v ++ " <- flag"
        RegExp r       -> "regexp"
        Explicit s     -> explicitCmb s ++ " \"" ++ s ++ "\""

  in do mapM production ps
        nothing

--eof
