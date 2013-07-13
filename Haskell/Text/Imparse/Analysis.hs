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
    Unanalyzed -> [R.Error]
    Normal -> []
    Duplicate -> [R.Duplicate]
    Unbound -> [R.Unbound]
    Unsupported -> [R.Error]

----------------------------------------------------------------
-- Analysis algorithms.

instance Analyze Parser where
  analyze (Parser a ims ps) =
    let 
        -- Check for duplicate productions.
        m = Map.fromListWith (+) [(e,1) | Production _ e _ <- ps]
        chk e = case Map.lookup e m of Just n -> if n > 1 then Duplicate else Normal ; _ -> Unanalyzed
        ps' = [Production (chk e) e cs | Production _ e cs <- ps]

        -- Check that all entities are bound.
        ps'' = entitiesBound ps'
        
    in Parser a ims ps''

entitiesBound :: [Production Analysis] -> [Production Analysis]
entitiesBound ps = 
  let es = [e | Production _ e _ <- ps]
      chkChoice c = case c of
        Choice c a es -> Choice c a (map chkElement es)
        _ -> c
      chkElement e = case e of
        NonTerminal _ e -> NonTerminal (if e `elem` es then Normal else Unbound) e
        _ -> e
  in [Production a e (map (map chkChoice) cs) | Production a e cs <- ps]


--eof
