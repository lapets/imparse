----------------------------------------------------------------
--
-- Imparse
--
-- Text/Imparse/Analysis.hs
--   Analyzer/validator for Imparse parsers.
--

----------------------------------------------------------------
-- 

module Text.Imparse.Analysis
  where

import qualified Data.Map as Map (fromListWith, lookup, Map)

import qualified Text.RichReports as R
import qualified StaticAnalysis.All as S

import qualified Text.Imparse.AbstractSyntax as A

----------------------------------------------------------------
-- Analysis data structure and instance declarations.

data Analysis =
    Unanalyzed
  | ParserA [ParserCategory] [NonTerminal] [Initial]
  | ProductionA [ProductionCategory] [NonTerminal] [Initial]
  | ChoicesA [ChoicesCategory] [NonTerminal] [Initial]
  | ChoiceA [ChoiceCategory] [NonTerminal] [Initial]
  | EntityA [EntityCategory]
  | Normal
  | Dup
  | Unbound
  | Unsupported
  deriving (Eq, Show)

data ParserCategory =
    Recursive
  | Linear
  | LeftLinear
  | RightLinear
  | CFG
  deriving (Eq, Show)

data ProductionCategory =
    ProductionCategory
  | Unreachable
  | Duplicate
  deriving (Eq, Show)

data ChoicesCategory =
    ChoicesCategory
  deriving (Eq, Show)

data ChoiceCategory =
    ChoiceUnknown
  | ChoiceBase
  | ChoiceRecursivePrefix
  | ChoiceRecursiveInfix
  | ChoiceRecursive
  | ChoiceOther
  deriving (Eq, Show)

data EntityCategory =
    EntityCategory
  deriving (Eq, Show)

type NonTerminal = String

data Initial =
    Initial
  deriving (Eq, Show)

instance S.Analysis Analysis where
  unanalyzed = Unanalyzed

----------------------------------------------------------------
-- Reporting of analysis results.

instance R.ToMessages Analysis where
  messages a = case a of
    Normal -> []
    Dup -> [R.Text "Duplicate."]
    Unbound -> [R.Text "Unbound."]
    Unsupported -> [R.Text "Unsupported."]
    Unanalyzed -> [R.Text "Unanalyzed."]

instance R.ToHighlights Analysis where
  highlights a = case a of
    Unanalyzed -> [R.HighlightError]
    Normal -> []
    Dup -> [R.HighlightDuplicate]
    Unbound -> [R.HighlightUnbound]
    Unsupported -> [R.HighlightError]

----------------------------------------------------------------
-- Collection of non-terminals.

class NonTerminals a where
  nonterminals :: a -> [NonTerminal]

instance NonTerminals (A.Production a) where
  nonterminals (A.Production _ e css) = concat $ map nonterminals css

instance NonTerminals (A.Choices a) where
  nonterminals (A.Choices _ cs) = concat $ map nonterminals cs

instance NonTerminals (A.Choice a) where
  nonterminals (A.Choice _ mc asc es) = concat $ map nonterminals es

instance NonTerminals (A.Element a) where
  nonterminals e = case e of
    A.NonTerminal _ e -> [e]
    A.Many e m ms -> nonterminals e
    A.Indented e -> nonterminals e
    _ -> []

----------------------------------------------------------------
-- Analysis algorithms.

analyze :: A.Parser Analysis -> A.Parser Analysis
analyze (A.Parser a ims ps) =
    let 
        productions :: [A.Production Analysis] -> [A.Production Analysis]
        productions ps =
          let es = [e | A.Production _ e _ <- ps]
          in
            [ A.Production a e [A.Choices a (map (choice es) cs) | A.Choices a cs <- css] 
            | A.Production a e css <- ps
            ]

        choice :: [A.NonTerminal] -> A.Choice Analysis -> A.Choice Analysis
        choice es (A.Choice a c asc es') = A.Choice a c asc (map (element es) es')

        element es e = case e of
          A.NonTerminal _ e -> A.NonTerminal (if e `elem` es then Normal else Unbound) e
          A.Many e n s      -> A.Many (element es e) n s
          A.Indented e      -> A.Indented (element es e)
          _ -> e

        -- Check for duplicate productions.
        m = Map.fromListWith (+) [(e,1) | A.Production _ e _ <- ps]
        chk e = case Map.lookup e m of Just n -> if n > 1 then Dup else Normal ; _ -> Unanalyzed
        ps' = [A.Production (chk e) e cs | A.Production _ e cs <- ps]

        -- Check that all entities are bound.
        ps'' = productions ps'

    in A.Parser a ims ps''




--eof
