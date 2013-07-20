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
import Data.List (nubBy)

import qualified Text.RichReports as R
import qualified Text.UXADT as U
import qualified StaticAnalysis.Annotated as A

----------------------------------------------------------------
-- Parser data structure.

type Import = String
type EntityName = String
type Constructor = String
type Minimum = Integer
type Separator = String
type TerminalString = String
type RegularExpression = String

data Parser a =
    Parser a [Import] [Production a]
  deriving Eq

data Production a =
    Production a EntityName [Choices a]
  deriving Eq

data Choices a =
    Choices a [Choice a]
  deriving Eq

data Choice a =
    Choice a (Maybe Constructor) Association [Element a]
  | PrecedenceSeparator a
  deriving Eq

data Association =
    AssocNone
  | AssocRight
  | AssocLeft
  | AssocFlat
  deriving Eq

data Element a =
    NonTerminal a EntityName
  | Many (Element a) Minimum (Maybe Separator)
  | Indented (Element a)
  | Terminal TerminalString
  | NewLine
  | StringLiteral
  | NaturalLiteral
  | DecimalLiteral
  | Identifier
  | Constructor
  | Flag
  | RegExp RegularExpression
  | ErrElement String
  deriving Eq

----------------------------------------------------------------
-- Static analysis annotation setting and retrieval.

instance A.Annotated Parser where
  annotate (Parser _ ms ps) a = Parser a ms ps
  annotation (Parser a ms ps) = a

----------------------------------------------------------------
-- Functions for inspecting parser instances.

isTerminal :: Element a -> Bool
isTerminal e = case e of
  NonTerminal _ _ -> False
  Many _ _ _      -> False
  Indented _      -> False
  ErrElement _    -> False
  _ -> True


isData :: Element a -> Bool
isData e = case e of
  NonTerminal _ _ -> True
  Many _ _ _      -> True
  Indented _      -> True
  StringLiteral   -> True
  NaturalLiteral  -> True
  DecimalLiteral  -> True
  Identifier      -> True
  Constructor     -> True
  Flag            -> True
  RegExp _        -> True
  _               -> False

eqTerminal :: Element a -> Element a -> Bool
eqTerminal t1 t2 = case (t1,t2) of
  (Terminal t1   , Terminal t2   ) -> t1 == t2
  (NewLine       , NewLine       ) -> True
  (StringLiteral , StringLiteral ) -> True
  (NaturalLiteral, NaturalLiteral) -> True
  (DecimalLiteral, DecimalLiteral) -> True
  (RegExp r1     , RegExp r2     ) -> r1 == r2
  _                                -> False

terminals :: Parser a -> [Element a]
terminals (Parser _ _ ps) =
  let cs = concat [cs | Production _ e css <- ps, Choices _ cs <- css]
  in nubBy eqTerminal $ concat [[e | e <- es, isTerminal e] | Choice _ _ _ es <- cs]

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
    Terminal t      -> U.C "Terminal" [U.S t]
    NewLine         -> U.C "Newline" []
    StringLiteral   -> U.C "StringLiteral" []
    NaturalLiteral  -> U.C "NaturalLiteral" []
    DecimalLiteral  -> U.C "DecimalLiteral" []
    RegExp r        -> U.C "RegExp" [U.S r]
    ErrElement s    -> U.C "ErrElement" [U.S s]

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
  show (PrecedenceSeparator a) = "^"
  show (Choice a c assoc es) = 
    (maybe "" id c) ++ " " ++ show assoc ++ " " ++ (join " " $ map show es)

instance Show (Element a) where
  show e = case e of
    NonTerminal _ n -> "`" ++ n
    Many e n ms     -> "`[" ++ show e ++ "/" ++ show n ++ (maybe "" (\s->"/" ++ show s) ms) ++ "]"
    Indented e      -> "`>" ++ show e ++ "<"
    Terminal t      -> t
    NewLine         -> "`_"
    StringLiteral   -> "`$"
    NaturalLiteral  -> "`#"
    DecimalLiteral  -> "`#.#"
    RegExp r        -> "`{" ++ r ++ "}"
    ErrElement s    -> "`!!!(" ++ s ++ ")!!!"
  
instance Show Association where
  show a = case a of
    AssocNone  -> "|"
    AssocRight -> ">"
    AssocLeft  -> "<"
    AssocFlat  -> "~"

--eof
