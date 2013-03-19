----------------------------------------------------------------
--
-- Imparse
--
-- Parser/AbstractSyntax.hs
--   Data structure for Imparse parsers.
--

----------------------------------------------------------------
-- 

module Parser.AbstractSyntax
  where

----------------------------------------------------------------
-- Parser data structure.

type EntityName = String
type Constructor = String

data Parser =
    Parser [Production]
  deriving Eq

data Production =
    Production EntityName [[Choice]]
  deriving Eq
  
data Choice =
    Choice (Maybe Constructor) Association [Element]
  deriving Eq

data Association =
    Bar
  | Right
  | Left
  | Flat
  deriving Eq

data Element =
    NonTerminal EntityName
  | Terminal
  | NewLine
  | Indent
  | Unindent
  | StringLiteral
  deriving Eq
    
----------------------------------------------------------------
-- Printing functions for the abstract syntax.

join xs xss = if length xss == 0 then "" else foldr (\s t->s++xs++t) (last xss) (init xss)

showParser (Parser ps) = join "\n\n" (map showProduction ps)

showProduction (Production en css) = en ++ " ::=\n" ++ join "\n" (map (map showChoice) css)

showChoice (Choice c a es) = c ++ " " ++ showAssociation a ++ " " ++ map showElement es

showElement _ = "<element>"

showAssociation a = case a of
  Bar -> "|"
  Right -> ">"
  Left -> "<"
  Flat -> "~"

--eof
