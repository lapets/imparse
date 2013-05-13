----------------------------------------------------------------
--
-- Imparse
--
-- Text/Imparse/AbstractSyntax.hs
--   Data structure for Imparse parser definitions.
--

----------------------------------------------------------------
-- 

module Text.Imparse.AbstractSyntax
  where

import Data.String.Utils (join)
import qualified Text.RichReports as R

----------------------------------------------------------------
-- Parser data structure.

type EntityName = String
type Constructor = String

data Parser a =
    Parser a [Production a]
  deriving Eq

data Production a =
    Production a EntityName [[Choice a]]
  deriving Eq
  
data Choice a =
    Choice (Maybe Constructor) Association [Element a]
  | PrecedenceSeparator
  deriving Eq

data Association =
    AssocNone
  | AssocRight
  | AssocLeft
  | AssocFlat
  deriving Eq

data Element a =
    NonTerminal a EntityName
  | Terminal String
  | NewLine
  | Indent
  | Unindent
  | StringLiteral
  | RegExp String
  | ErrElement String
  deriving Eq
    
----------------------------------------------------------------
-- Functions for converting a parser abstract syntax instance
-- into a rich report.

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Parser a) where
  report (Parser _ ps) = R.Finalize $ R.Conc [R.report p | p <- ps]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Production a) where
  report (Production a e css) = 
    R.Block [] [] [
      R.Line [] [R.Space],
      R.C R.Variable (R.highlights a) (R.messages a) e, R.Text "::=",
      R.BlockIndent [] [] [
        R.Table [
          R.Intersperse 
            (R.Row [R.Field (R.Conc []), R.Field (R.Text "^"), R.Field (R.Conc [])]) 
            [R.Conc [R.report c | c <- cs] | cs <- css]
        ]
      ]
    ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Choice a) where
  report (PrecedenceSeparator) = R.Text "^"
  report (Choice c a es) =
    R.Row [
      R.Field (maybe (R.Conc []) R.Text c), 
      R.Field (R.Text (show a)), 
      R.Field (R.Span [] [R.Text "testing"] [R.Conc [R.report e | e <- es]])
      ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Element a) where
  report r = case r of
    NonTerminal a n -> R.C R.Variable (R.highlights a) (R.messages a) $ "`" ++ n
    Terminal t      -> R.C R.Keyword [] [] $ t
    NewLine         -> R.C R.Literal [] [] $ "`_"
    Indent          -> R.C R.Literal [] [] $ "`>"
    Unindent        -> R.C R.Literal [] [] $ "`<"
    StringLiteral   -> R.C R.Literal [] [] $ "`$"
    RegExp r        -> R.Text $ "`[" ++ r ++ "]"
    ErrElement s    -> R.Text $ "`![" ++ s ++ "]!"

----------------------------------------------------------------
-- Functions for converting a parser into an ASCII string.

instance Show (Parser a) where
  show = showParser

showParser (Parser _ ps) = join "\n\n" (map showProduction ps) ++ "\n"

showProduction (Production a en css) = 
  en ++ " ::=\n  " ++ join "\n  ^\n  " [join "\n  " $ map showChoice cs | cs <- css]

showChoice (PrecedenceSeparator) = "^"
showChoice (Choice c a es) = 
  (maybe "" id c) ++ " " ++ show a ++ " " ++ (join " " $ map showElement es)

showElement e = case e of
  NonTerminal _ n -> "`" ++ n
  NewLine         -> "`_"
  Indent          -> "`>"
  Unindent        -> "`<"
  Terminal t      -> t
  StringLiteral   -> "`$"
  RegExp r        -> "`[" ++ r ++ "]"
  ErrElement s    -> "`![" ++ s ++ "]!"
  
instance Show Association where
  show a = case a of
    AssocNone  -> "|"
    AssocRight -> ">"
    AssocLeft  -> "<"
    AssocFlat  -> "~"

--eof
