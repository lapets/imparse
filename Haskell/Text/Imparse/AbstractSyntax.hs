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
import Data.List (nub)

import qualified Text.RichReports as R
import qualified Text.UXADT as U
import qualified StaticAnalysis.All as A

----------------------------------------------------------------
-- Parser data structure.

type Import = String
type NonTerminal = String
type Constructor = String

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
  | Many (Element a) Minimum (Maybe Separator)
  | Indented (Element a)
  | Terminal Terminal
  | Error String
  deriving Eq

data Terminal =
    Explicit String
  | NewLine
  | StringLiteral
  | NaturalLiteral
  | DecimalLiteral
  | Identifier
  | Constructor
  | Flag
  | RegExp RegularExpression
  deriving Eq

----------------------------------------------------------------
-- Static analysis annotation setting and retrieval.

instance A.Annotated Parser where
  annotate (Parser _ ms ps) a = Parser a ms ps
  annotation (Parser a ms ps) = a

instance A.Annotated Production where
  annotate (Production _ e css) a = Production a e css
  annotation (Production a _ _) = a

instance A.Annotated Choices where
  annotate (Choices _ cs) a = Choices a cs
  annotation (Choices a _) = a

instance A.Annotated Choice where
  annotate (Choice _ mc asc es) a = Choice a mc asc es
  annotation (Choice a _ _ _) = a

instance A.Annotated Element where
  annotate e a = case e of
    NonTerminal _ e -> NonTerminal a e
    Many e m ms -> Many (A.annotate e a) m ms
    Indented e -> Indented $ A.annotate e a
    _ -> e
  annotation e = case e of
    NonTerminal a _ -> a
    Many e _ _ -> A.annotation e
    Indented e -> A.annotation e
    _ -> A.unanalyzed

----------------------------------------------------------------
-- Functions for inspecting parser instances.

isData :: Element a -> Bool
isData e = case e of
  NonTerminal _ _         -> True
  Many _ _ _              -> True
  Indented _              -> True
  Terminal StringLiteral  -> True
  Terminal NaturalLiteral -> True
  Terminal DecimalLiteral -> True
  Terminal Identifier     -> True
  Terminal Constructor    -> True
  Terminal Flag           -> True
  Terminal (RegExp _)     -> True
  _               -> False

terminals :: Parser a -> [Terminal]
terminals (Parser _ _ ps) = nub $
  [ t | 
    Production _ e css <- ps, 
    Choices _ cs <- css, 
    Choice _ _ _ es <- cs, 
    Terminal t <- es
  ]

----------------------------------------------------------------
-- Functions for converting a parser into a UXADT instance string.

instance U.ToUXADT (Parser a) where
  uxadt (p@(Parser _ _ ps)) = 
    U.C "Parser" [
      U.C "Productions" [U.L [U.uxadt p | p <- ps]],
      U.C "Terminals" [U.L [U.uxadt t | t <- terminals p]]
    ]

instance U.ToUXADT (Production a) where
  uxadt (Production _ en css) = U.C "Production" [U.S en, U.uxadt css]

instance U.ToUXADT (Choices a) where
  uxadt (Choices _ cs) = U.C "Choices" [U.uxadt c | c <- cs]

instance U.ToUXADT (Choice a) where
  uxadt (Choice _ c _ es) = U.C "Choice" [maybe U.None U.S c, U.uxadt es]

instance U.ToUXADT (Element a) where
  uxadt e = case e of
    NonTerminal _ n -> U.C "NonTerminal" [U.S n]
    Many e n ms     -> U.C "Many" $ [U.uxadt e, U.I (fromInteger n)] ++ maybe [] (\s -> [U.S s]) ms
    Indented e      -> U.C "Indented" [U.uxadt e]
    Terminal t      -> U.C "Terminal" [U.uxadt t]
    Error s         -> U.C "Error" [U.S s]

instance U.ToUXADT Terminal where
  uxadt t = case t of
    Explicit s      -> U.C "Explicit" [U.S s]
    NewLine         -> U.C "Newline" []
    StringLiteral   -> U.C "StringLiteral" []
    NaturalLiteral  -> U.C "NaturalLiteral" []
    DecimalLiteral  -> U.C "DecimalLiteral" []
    Identifier      -> U.C "Identifier" []
    Constructor     -> U.C "Constructor" []
    Flag            -> U.C "Flag" []
    RegExp r        -> U.C "RegExp" [U.S r]

----------------------------------------------------------------
-- Functions for converting a parser into an ASCII string.

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
  show e = case e of
    NonTerminal _ n -> "`" ++ n
    Many e n ms     -> "`[" ++ show e ++ "/" ++ show n ++ (maybe "" (\s->"/" ++ show s) ms) ++ "]"
    Indented e      -> "`>" ++ show e ++ "<"
    Terminal t      -> show t
    Error s         -> "`!!!(" ++ s ++ ")!!!"

instance Show Terminal where
  show t = case t of
    Explicit s      -> s
    NewLine         -> "`_"
    StringLiteral   -> "`$"
    NaturalLiteral  -> "`#"
    DecimalLiteral  -> "`#.#"
    Identifier      -> "`id"
    Constructor     -> "`con"
    Flag            -> "`flag"
    RegExp r        -> "`{" ++ r ++ "}"

--eof
