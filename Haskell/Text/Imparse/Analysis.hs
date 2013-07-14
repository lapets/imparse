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

import Text.Imparse.AbstractSyntax

----------------------------------------------------------------
-- Data structure and class.

data Variables =
    Variables
  deriving (Eq, Show)

data NonTerminals =
    NonTerminals
  deriving (Eq, Show)

data Initials =
    Initials
  deriving (Eq, Show)

data Analysis2 =
  Analysis {
    variables :: Variables,
    nonterminals :: NonTerminals,
    initials :: Initials
  } deriving (Eq, Show)

data ChoiceKind =
    ChoiceUnknown
  | ChoiceBase
  | ChoiceRecursivePrefix
  | ChoiceRecursiveInfixLeftAssoc
  | ChoiceRecursiveInfixRightAssoc
  | ChoiceRecursiveInfixFlatAssoc
  | ChoiceRecursive
  | ChoiceOther
  deriving (Eq, Show)

data Analysis =
    Unanalyzed
  | Normal
  | Duplicate
  | Unbound
  | Unsupported
  deriving (Eq, Show)

data Grammar =
    Recursive
  | Linear
  | LeftLinear
  | RightLinear
  | CFG
  deriving (Eq, Show)

class Analyze a where
  analyze :: a Analysis -> a Analysis

----------------------------------------------------------------
-- Reporting of analysis results.

instance R.ToMessages Analysis where
  messages a = case a of
    Normal -> []
    Duplicate -> [R.Text "Duplicate."]
    Unbound -> [R.Text "Unbound."]
    Unsupported -> [R.Text "Unsupported."]
    Unanalyzed -> [R.Text "Unanalyzed."]

instance R.ToHighlights Analysis where
  highlights a = case a of
    Unanalyzed -> [R.HighlightError]
    Normal -> []
    Duplicate -> [R.HighlightDuplicate]
    Unbound -> [R.HighlightUnbound]
    Unsupported -> [R.HighlightError]

----------------------------------------------------------------
-- Analysis algorithms.

instance Analyze Parser where
  analyze (Parser a ims ps) =
    let 
        productions :: [Production Analysis] -> [Production Analysis]
        productions ps = 
          let es = [e | Production _ e _ <- ps]
          in [Production a e (map (map (choice es)) cs) | Production a e cs <- ps]

        choice es c = case c of
          Choice c a es' -> Choice c a (map (element es) es')
          _              -> c

        element es e = case e of
          NonTerminal _ e -> NonTerminal (if e `elem` es then Normal else Unbound) e
          Many e n s      -> Many (element es e) n s
          Indented e      -> Indented (element es e)
          _ -> e

        -- Check for duplicate productions.
        m = Map.fromListWith (+) [(e,1) | Production _ e _ <- ps]
        chk e = case Map.lookup e m of Just n -> if n > 1 then Duplicate else Normal ; _ -> Unanalyzed
        ps' = [Production (chk e) e cs | Production _ e cs <- ps]

        -- Check that all entities are bound.
        ps'' = productions ps'

    in Parser a ims ps''

--eof
