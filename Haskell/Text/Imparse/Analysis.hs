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
-- Analysis data structure, instance declarations, accessors,
-- and mutators.

type InitialNonTerminals = [A.NonTerminal]
type InitialTerminals = [A.Terminal]
type ReachableNonTerminals = [A.NonTerminal]
type Characterization = 
  (InitialTerminals, InitialNonTerminals, ReachableNonTerminals)

data Analysis =
    Unanalyzed
  | Analyzed [Tag] Characterization
  deriving (Eq, Show)

data Tag =
    NonTerminalUnbound
  | ChoiceBase
  | ChoiceRecursive
  | ChoiceRecursivePrefix
  | ChoiceRecursiveInfix
  | ChoicesRecursivePrefixInfix
  | ProductionDuplicate
  | ProductionUnreachable
  | GrammarRecursive
  | GrammarNonRecursive
  | GrammarLinear
  | GrammarLeftLinear
  | GrammarRightLinear
  | GrammarCFG
  deriving (Eq, Show)

instance S.Analysis Analysis where
  unanalyzed = Unanalyzed

mapCmb :: (a -> (a, Characterization)) -> [a] -> ([a], Characterization)
mapCmb f xs = (xs', combine cs)
  where 
    combine :: [Characterization] -> Characterization
    combine c = (concat x, concat y, concat z) where (x,y,z) = unzip3 c
    (xs', cs) = unzip $ map f xs

tag :: Analysis -> [Tag] -> Analysis
tag a ts' = case a of 
  Analyzed ts c -> Analyzed (ts' ++ ts) c
  Unanalyzed    -> Analyzed ts' ([], [], [])

----------------------------------------------------------------
-- Reporting of analysis results.

instance R.ToMessages Analysis where
  messages a = case a of
    Unanalyzed -> [R.Text "Unanalyzed."]
    Analyzed ts c -> concat $ map R.messages ts

instance R.ToHighlights Analysis where
  highlights a = case a of
    Unanalyzed -> [R.HighlightError]
    Analyzed ts c -> concat $ map R.highlights ts

instance R.ToMessages Tag where
  messages t = case t of
    NonTerminalUnbound  -> [R.Text "Unbound."]
    ChoiceBase          -> [R.Text "Base."]
    ProductionDuplicate -> [R.Text "Duplicate."]
    _ -> []

instance R.ToHighlights Tag where
  highlights t = case t of
    NonTerminalUnbound -> [R.HighlightUnbound]
    ProductionDuplicate -> [R.HighlightDuplicate]
    _ -> []

----------------------------------------------------------------
-- Baseline analysis (initial non-/terminals and reachable
-- non-terminals) and its closure (fully recursive
-- characterization of initial and reachable non-/terminals).

baseline :: A.Parser Analysis -> A.Parser Analysis
baseline (A.Parser a ims ps) = A.Parser (Analyzed [] r) ims ps where
  (ps', r) = mapCmb production ps

  production (A.Production _ e css) = (A.Production (Analyzed [] r) e css', r)
    where (css', r) = mapCmb choices css

  choices (A.Choices _ cs) = (A.Choices (Analyzed [] r) cs', r)
    where (cs', r) = mapCmb choice cs
  
  choice (A.Choice _ mc asc (es@(e:_))) = (A.Choice (Analyzed [] r) mc asc es, r)
    where r = (terminals e, nonterminals e, concat $ map reachable es)

  terminals e = case e of A.Terminal t -> [t] ; _ -> []  

  nonterminals e = case e of
    A.NonTerminal _ e -> [e]
    A.Many e m ms     -> nonterminals e
    _                 -> []

  reachable e = case e of
    A.NonTerminal _ e -> [e]
    A.Many e m ms     -> reachable e
    A.Indented e      -> reachable e
    _                 -> []

closure :: A.Parser Analysis -> A.Parser Analysis
closure (A.Parser a ims ps) = A.Parser a ims ps where
  () = ()

----------------------------------------------------------------
-- Property derivation and tagging algorithms.

tagging :: A.Parser Analysis -> A.Parser Analysis
tagging (A.Parser a ims ps) = A.Parser a ims (map production ps) where
  production (A.Production a e css) = A.Production (tag a ts) e $ map choices css
    where ts = []

  choices (A.Choices a cs) = A.Choices (tag a ts) $ map choice cs
    where ts = []
  
  choice (A.Choice a mc asc es) = A.Choice (tag a ts') mc asc es
    where ts' = 
            if length es == length [e | e@(A.Terminal _) <- es] then [ChoiceBase] else []

analyze :: A.Parser Analysis -> A.Parser Analysis
analyze parser =
    let 
        (A.Parser a ims ps) = tagging $ closure $ baseline parser
    
        productions :: [A.Production Analysis] -> [A.Production Analysis]
        productions ps =
          let es = [e | A.Production _ e _ <- ps]
          in
            [ A.Production a e 
                [A.Choices a (map (choice es) cs) | A.Choices a cs <- css]
            | A.Production a e css <- ps
            ]

        choice :: [A.NonTerminal] -> A.Choice Analysis -> A.Choice Analysis
        choice es (A.Choice a c asc es') = A.Choice a c asc (map (element es) es')

        element es e = case e of
          A.NonTerminal a e -> A.NonTerminal (tag a $ if e `elem` es then [] else [NonTerminalUnbound]) e
          A.Many e n s      -> A.Many (element es e) n s
          A.Indented e      -> A.Indented (element es e)
          _ -> e

        -- Check for duplicate productions.
        m = Map.fromListWith (+) [(e,1) | A.Production _ e _ <- ps]
        chkDups e = case Map.lookup e m of Just n -> if n > 1 then [ProductionDuplicate] else [] ; _ -> []
        ps' = [A.Production (tag a (chkDups e)) e cs | A.Production a e cs <- ps]

        -- Check that all entities are bound.
        ps'' = productions ps'

    in A.Parser a ims ps''


--eof
