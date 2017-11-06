----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Text\/Imparse\/AbstractSyntax.hs@
--
--   Data structure for Imparse parser definitions.
--

----------------------------------------------------------------
-- 

module Text.Imparse.AbstractSyntax
  where

import Data.Char (isAlpha)
import Data.List (nub)
import Data.String.Utils (join)

import qualified Text.RichReports as R
import qualified Text.UxADT as U
import qualified StaticAnalysis.Annotate as A
import qualified StaticAnalysis.Analyze as A
import qualified StaticAnalysis.Analysis as A

----------------------------------------------------------------
-- | Parser data structure.

type Import = String
type NonTerminal = String
type Constructor = String
type WhitespaceParse = Bool

data Parser a =
    Parser a [Import] [Production a]
  deriving Eq

data Production a =
    Production a NonTerminal [Choices a]
  deriving Eq

data Choices a =
    Choices a [Choice a]
  deriving Eq

data Choice a = 
    Choice a (Maybe Constructor) Association [Element a]
  deriving Eq

type Minimum = Integer
type Separator = String
type TerminalString = String
type RegularExpression = String

data Association =
    AssocNone
  | AssocRight
  | AssocLeft
  | AssocFlat
  deriving Eq

data Element a =
    NonTerminal a NonTerminal
  | Many (Element a) (Maybe Separator)
  | May (Element a)
  | Indented WhitespaceParse (Element a)
  | Terminal Terminal
  | Error String
  deriving Eq

data Terminal =
    Explicit String
  | StringLiteral
  | NaturalLiteral
  | DecimalLiteral
  | Identifier
  | Constructor
  | Flag
  | RegExp RegularExpression
  deriving Eq

----------------------------------------------------------------
-- | Static analysis annotation setting and retrieval.

instance A.Annotate Parser where
  annotate a (Parser _ ms ps) = Parser a ms ps
  annotation (Parser a ms ps) = a

instance A.Annotate Production where
  annotate a (Production _ e css) = Production a e css
  annotation (Production a _ _) = a

instance A.Annotate Choices where
  annotate a (Choices _ cs) = Choices a cs
  annotation (Choices a _) = a

instance A.Annotate Choice where
  annotate a (Choice _ mc asc es) = Choice a mc asc es
  annotation (Choice a _ _ _) = a

instance A.Annotate Element where
  annotate a e = case e of
    NonTerminal _ e -> NonTerminal a e
    Many e ms       -> Many (A.annotate a e) ms
    May e           -> May (A.annotate a e)
    Indented w e    -> Indented w $ A.annotate a e
    _ -> e
  annotation e = case e of
    NonTerminal a _ -> a
    Many e _        -> A.annotation e
    May e           -> A.annotation e
    Indented w e    -> A.annotation e
    _               -> A.unanalyzed

----------------------------------------------------------------
-- | Functions for inspecting parser instances.

isOp :: String -> Bool
isOp s = not (s `elem` ["(",")","[","]","{","}"]) && not (and (map isAlpha s))

isData :: Element a -> Bool
isData e = case e of
  NonTerminal _ _         -> True
  Many _ _                -> True
  May _                   -> True
  Indented _ _            -> True
  Terminal StringLiteral  -> True
  Terminal NaturalLiteral -> True
  Terminal DecimalLiteral -> True
  Terminal Identifier     -> True
  Terminal Constructor    -> True
  Terminal Flag           -> True
  Terminal (RegExp _)     -> True
  _                       -> False

terminals :: Parser a -> [Terminal]
terminals (Parser _ _ ps) =
  nub $ 
    [t | Production _ e css <- ps, Choices _ cs <- css, Choice _ _ _ es <- cs, Terminal t <- es]

productionNonTerminal :: Production a -> NonTerminal
productionNonTerminal (Production _ nt _) = nt

----------------------------------------------------------------
-- | Functions for converting a parser into a UXADT instance string.

instance U.ToUxADT (Parser a) where
  uxadt (p@(Parser _ _ ps)) = 
    U.C "Parser" [
      U.C "Productions" [U.L [U.uxadt p | p <- ps]],
      U.C "Terminals" [U.L [U.uxadt t | t <- terminals p]]
    ]

instance U.ToUxADT (Production a) where
  uxadt (Production _ en css) = U.C "Production" [U.S en, U.uxadt css]

instance U.ToUxADT (Choices a) where
  uxadt (Choices _ cs) = U.C "Choices" [U.uxadt c | c <- cs]

instance U.ToUxADT (Choice a) where
  uxadt (Choice _ c _ es) = U.C "Choice" [maybe U.None U.S c, U.uxadt es]

instance U.ToUxADT (Element a) where
  uxadt e = case e of
    NonTerminal _ n -> U.C "NonTerminal" [U.S n]
    Many e ms       -> U.C "Many" $ [U.uxadt e] ++ maybe [] (\s -> [U.S s]) ms
    May e           -> U.C "May" [U.uxadt e]
    Indented w e    -> U.C "Indented" [U.uxadt w, U.uxadt e]
    Terminal t      -> U.C "Terminal" [U.uxadt t]
    Error s         -> U.C "Error" [U.S s]

instance U.ToUxADT Terminal where
  uxadt t = case t of
    Explicit s      -> U.C "Explicit" [U.S s]
    StringLiteral   -> U.C "StringLiteral" []
    NaturalLiteral  -> U.C "NaturalLiteral" []
    DecimalLiteral  -> U.C "DecimalLiteral" []
    Identifier      -> U.C "Identifier" []
    Constructor     -> U.C "Constructor" []
    Flag            -> U.C "Flag" []
    RegExp r        -> U.C "RegExp" [U.S r]

----------------------------------------------------------------
-- | Functions for converting a parser into an ASCII string.

instance Show (Parser a) where
  show (Parser _ _ ps) = join "\n\n" (map show ps) ++ "\n"

instance Show (Production a) where
  show (Production a en css) = 
    en ++ " ::=\n  " ++ join "\n  ^\n  " [show cs | cs <- css]

instance Show (Choices a) where
  show (Choices a cs) = join "\n  " $ map show cs

instance Show (Choice a) where
  show (Choice a c assoc es) = 
    (maybe "" id c) ++ " " ++ show assoc ++ " " ++ (join " " $ map show es)

instance Show Association where
  show a = case a of
    AssocNone  -> "|"
    AssocRight -> ">"
    AssocLeft  -> "<"
    AssocFlat  -> "~"

instance Show (Element a) where
  show (Terminal t) = show t
  show (Error s)    = "`!!!_" ++ s ++ "_!!!"
  show e            = 
    let rec e = case e of
          NonTerminal _ nt -> nt
          Many e ms        -> "[" ++ rec e ++ (maybe "" (\s->"/" ++ show s) ms) ++ "]"
          May e            -> "(" ++ rec e ++ ")"
          Indented w e     -> if w then ">>" ++ rec e ++ "<<" else ">" ++ rec e ++ "<"
    in "`" ++ rec e

instance Show Terminal where
  show t = case t of
    Explicit s      -> s
    StringLiteral   -> "`$"
    NaturalLiteral  -> "`#"
    DecimalLiteral  -> "`#.#"
    Identifier      -> "`id"
    Constructor     -> "`con"
    Flag            -> "`flag"
    RegExp r        -> "`{" ++ r ++ "}"

--eof
