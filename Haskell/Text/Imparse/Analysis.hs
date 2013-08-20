----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Text\/Imparse\/Analysis.hs@
--
--   Analyzer/validator for Imparse parsers.
--

----------------------------------------------------------------
--

module Text.Imparse.Analysis
  where

import Data.List (nub, intersect)
import Data.Maybe (isJust)
import qualified Data.Map as Map (fromListWith, lookup, Map)

import qualified Text.RichReports as R
import qualified StaticAnalysis.All as S

import qualified Text.Imparse.AbstractSyntax as A
import Text.Imparse.Report

----------------------------------------------------------------
-- | Analysis data structure, instance declarations, accessors,
--   and mutators.

type InitialNonTerminals = [A.NonTerminal]
type InitialTerminals = [A.Terminal]
type ReachableNonTerminals = [A.NonTerminal]
type Characterization = (InitialTerminals, InitialNonTerminals, ReachableNonTerminals)

data Analysis =
    Analyzed [Tag] Characterization
  deriving (Eq, Show)

data Tag =
    GrammarRecursive
  | GrammarNonRecursive
  | GrammarLinear
  | GrammarLeftLinear
  | GrammarRightLinear
  | GrammarCFG
  | ProductionBase
  | ProductionNonRecursive
  | ProductionRecursive
  | ProductionInfixPrefixThenDeterministic
  | ProductionDeterministic
  | ProductionDuplicate
  | ProductionUnreachable
  | ChoicesBase
  | ChoicesDeterministic
  | ChoicesNonRecursive
  | ChoicesRecursive
  | ChoicesRecursivePrefixInfix
  | ChoiceBase
  | ChoiceNonRecursive
  | ChoiceRecursive
  | ChoiceRecursivePrefix
  | ChoiceRecursiveInfix
  | ChoiceConstructorDuplicate
  | NonTerminalUnbound
  deriving (Eq, Show)

instance S.Analysis Analysis where
  unanalyzed = Analyzed [] ([], [], [])

tag :: Analysis -> [Tag] -> Analysis
tag a ts' = case a of 
  Analyzed ts c -> Analyzed (nub $ ts' ++ ts) c

tags :: S.Annotate a => a Analysis -> [Tag]
tags d = let Analyzed ts _ = S.annotation d in ts

initialTerminals :: S.Annotate a => a Analysis -> InitialTerminals
initialTerminals d = let Analyzed _ (ts, _, _) = S.annotation d in ts

initialNonTerminals :: S.Annotate a => a Analysis -> InitialNonTerminals
initialNonTerminals d = let Analyzed _ (_, ns, _) = S.annotation d in ns

reachable :: S.Annotate a => a Analysis -> ReachableNonTerminals
reachable d = let Analyzed _ (_, _, rns) = S.annotation d in rns

characterization :: S.Annotate a => a Analysis -> Characterization
characterization d = let Analyzed _ c = S.annotation d in c

combine :: [Characterization] -> Characterization
combine c = (nub $ concat x, nub $ concat y, nub $ concat z) where (x,y,z) = unzip3 c

mapCmb :: (a -> (a, Characterization)) -> [a] -> ([a], Characterization)
mapCmb f xs = let (xs', cs) = unzip $ map f xs in (xs', combine cs)

----------------------------------------------------------------
-- | Reporting of analysis results.

instance R.ToMessages Analysis where
  messages a = case a of
    Analyzed []   ([], [], [] ) -> [R.Text "Unanalyzed."]
    Analyzed tags (ts, ns, rns) -> [
        R.Table [
          R.Row [ R.Field (R.Text "term.:"), R.Field (R.Intersperse (R.Text ",") $ map R.report ts) ],
          R.Row [ R.Field (R.Text "non-term.:"), R.Field (R.Intersperse (R.Text ",") $ map R.Text ns) ],
          R.Row [ R.Field (R.Text "reach.:"), R.Field (R.Intersperse (R.Text ",") $ map R.Text rns) ],
          R.Row [ R.Field (R.Text "prop.:"), R.Field (R.Intersperse (R.Text ",") $ concat $ map R.messages tags) ]
        ]
      ] 

instance R.ToHighlights Analysis where
  highlights a = case a of
    Analyzed [] ([], [], []) -> [R.HighlightError]
    Analyzed tags c          -> concat $ map R.highlights tags

instance R.ToMessages Tag where
  messages t = case t of
    ProductionBase              -> [R.Text "Base"]
    ProductionNonRecursive      -> [R.Text "NonRecursive"]
    ProductionRecursive         -> [R.Text "Recursive"]
    ProductionInfixPrefixThenDeterministic -> [R.Text "InfixPrefixThenDeterministic"]
    ProductionDeterministic     -> [R.Text "Deterministic"]
    ProductionDuplicate         -> [R.Text "Duplicate"]
    ProductionUnreachable       -> [R.Text "Unreachable"]
    ChoicesBase                 -> [R.Text "Base"]
    ChoicesDeterministic        -> [R.Text "Deterministic"]
    ChoicesNonRecursive         -> [R.Text "NonRecursive"]
    ChoicesRecursive            -> [R.Text "Recursive"]
    ChoicesRecursivePrefixInfix -> [R.Text "RecursivePrefixInfix"]
    ChoiceBase                  -> [R.Text "Base"]
    ChoiceNonRecursive          -> [R.Text "NonRecursive"]
    ChoiceRecursive             -> [R.Text "Recursive"]
    ChoiceRecursivePrefix       -> [R.Text "RecursivePrefix"]
    ChoiceRecursiveInfix        -> [R.Text "RecursiveInfix"]
    ChoiceConstructorDuplicate  -> [R.Text "ConstructorDuplicate"]
    NonTerminalUnbound          -> [R.Text "Unbound"]
    _ -> []

instance R.ToHighlights Tag where
  highlights t = case t of
    NonTerminalUnbound         -> [R.HighlightUnbound]
    ProductionDuplicate        -> [R.HighlightDuplicate]
    ProductionUnreachable      -> [R.HighlightUnreachable]
    ChoiceConstructorDuplicate -> [R.HighlightError]
    _ -> []

----------------------------------------------------------------
-- | Baseline analysis (initial non-/terminals and reachable
--   non-terminals) and its closure (fully recursive
--   characterization of initial and reachable non-/terminals).

baseline :: A.Parser Analysis -> A.Parser Analysis
baseline (A.Parser a ims ps) = A.Parser (Analyzed [] r) ims ps' where
  (ps', r) = mapCmb production ps

  production (A.Production _ e css) = (A.Production (Analyzed [] r) e css', r)
    where (css', r) = mapCmb choices css

  choices (A.Choices _ cs) = (A.Choices (Analyzed [] r) cs', r)
    where (cs', r) = mapCmb choice cs
  
  choice (A.Choice _ mc asc (es@(e:_))) = (A.Choice (Analyzed [] r) mc asc es, r)
    where r = (nub $ terminals e, nub $ nonterminals e, nub $ concat $ map reachable es)

  terminals e = case e of A.Terminal t -> [t] ; _ -> []  

  nonterminals e = case e of
    A.NonTerminal _ e -> [e]
    A.Many e ms       -> nonterminals e
    A.May e           -> nonterminals e
    _                 -> []

  reachable e = case e of
    A.NonTerminal _ e -> [e]
    A.Many e ms       -> reachable e
    A.May e           -> reachable e
    A.Indented w e    -> reachable e
    _                 -> []

closure :: A.Parser Analysis -> A.Parser Analysis
closure (A.Parser a ims ps) = A.Parser a ims ps'' where
  ps'' = 
    [ A.Production a e
        [ let cs' =
                [ let es' =
                        [ let sub e = case e of
                                A.NonTerminal _ e -> 
                                  A.NonTerminal (
                                        let l = concat $ map (lookP e) ps' 
                                        in if length l > 0 then (Analyzed [] (head l)) else S.unanalyzed
                                      )
                                      e
                                A.Many e ms       -> A.Many (sub e) ms
                                A.May e           -> A.May (sub e)
                                A.Indented w e    -> A.Indented w (sub e)
                                _                 -> e
                          in sub e
                        | e <- es 
                        ]
                      (ts', ns', _) = characterization (head es')
                      rs' = concat $ map reachable es'
                  in A.Choice (Analyzed [] (nub $ ts'++ts, nub $ ns'++ns, nub $ rs'++rs)) con asc es'
                | A.Choice (Analyzed _ (ts, ns, rs)) con asc es <- cs
                ]
          in A.Choices (Analyzed [] (combine $ [c] ++ map characterization cs')) cs'
        | A.Choices (Analyzed _ c) cs <- css
        ]
    | A.Production a e css <- ps'
    ]
  ps' = (foldr (.) id $ take (length ps) $ repeat step) ps
  step ps =
    [ let (_, _   , rss') = unzip3 $ map (look rs) ps
          (_, nss', _   ) = unzip3 $ map (look ns) ps
          (ns', rs')   = (concat nss', concat rss')
          ts' = nub $ (ts ++ concat (map (lookTs (ns ++ ns')) ps))
      in A.Production (Analyzed tags (ts', nub $ ns ++ ns', nub $ rs ++ rs')) e css
    | A.Production (Analyzed tags (ts, ns, rs)) e css <- ps
    ]
  look es (A.Production (Analyzed _ c) e _) = if e `elem` es then c else ([], [], [])
  lookTs es (A.Production (Analyzed _ (ts, ns, rs)) e _) = if e `elem` es then ts else []
  lookP e' (A.Production (Analyzed _ c) e _) = if e == e' then [c] else []

----------------------------------------------------------------
-- | Property derivation and tagging algorithms.

tagging :: A.Parser Analysis -> A.Parser Analysis
tagging (A.Parser a ims ps) = A.Parser a ims (map production ps) where
  production (A.Production a e css) = A.Production (tag a ts) e css'
    where
      css' = map (choices e) css
      ts = (if and [ChoicesBase `elem` tags cs | cs <- css'] then [ProductionBase] else [])
        ++ (if and [ChoicesNonRecursive `elem` tags cs | cs <- css'] then [ProductionNonRecursive] else [])
        ++ (if or [ChoicesRecursive `elem` tags cs | cs <- css'] then [ProductionRecursive] else [])
        ++ (if or [ChoicesRecursive `elem` tags cs | cs <- css'] then [ProductionRecursive] else [])
        ++ (let pat [ts]     = ChoicesDeterministic `elem` ts || ChoicesNonRecursive `elem` ts
                pat (ts:tss) = ChoicesRecursivePrefixInfix `elem` ts && pat tss
            in if length css' > 1 && pat [tags cs | cs <- css'] then [ProductionInfixPrefixThenDeterministic] else []
           )

  choices e (cc@(A.Choices a cs)) = A.Choices (tag a ts) cs'
    where
      cs' = map (choice e) cs
      ts = (if and [ChoiceBase `elem` tags c | c <- cs'] then [ChoicesBase] else [])
        ++ [if e `elem` reachable cc then ChoicesRecursive else ChoicesNonRecursive]
        ++ (if and [initialTerminals (cs'!!i) `intersect` initialTerminals (cs'!!j) == [] | i <- [0..length cs'-1], j <- [0..i-1]] then
              [ChoicesDeterministic] 
            else
              []
           )
        ++ (if and [ChoiceRecursivePrefix `elem` tags c || ChoiceRecursiveInfix `elem` tags c | c <- cs'] then 
              [ChoicesRecursivePrefixInfix]
            else 
              []
           )

  choice e (c@(A.Choice a mc asc es)) = A.Choice (tag a ts') mc asc es
    where 
      ts' = (if length es == length [e | e@(A.Terminal _) <- es] then [ChoiceBase] else [])
         ++ [if e `elem` reachable c then ChoiceRecursive else ChoiceNonRecursive]
         ++ (case es of 
               [A.Terminal (A.Explicit _), A.NonTerminal _ nt] -> 
                 if nt == e && isJust mc then [ChoiceRecursivePrefix] else []
               [A.NonTerminal _ nt1, A.Terminal (A.Explicit _), A.NonTerminal _ nt2] -> 
                 if nt1 == e && nt2 == e && isJust mc then [ChoiceRecursiveInfix] else []
               _ -> []
            )

analyze :: A.Parser Analysis -> A.Parser Analysis
analyze parser =
    let 
        (A.Parser a ims ps) = tagging $ closure $ baseline parser

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
          A.NonTerminal a e -> A.NonTerminal (tag a $ if e `elem` es then [] else [NonTerminalUnbound]) e
          A.Many e s        -> A.Many (element es e) s
          A.May e           -> A.May (element es e)
          A.Indented w e    -> A.Indented w (element es e)
          _ -> e

        -- Check for duplicate productions non-terminal names.
        m = Map.fromListWith (+) [(e,1) | A.Production _ e _ <- ps]
        chkDups top e = (case Map.lookup e m of Just n -> if n > 1 then [ProductionDuplicate] else [] ; _ -> [])
                     ++ (if not (e `elem` reachable top) && (e /= A.productionNonTerminal top) then [ProductionUnreachable] else [])
        ps' = [A.Production (tag a (chkDups (head ps) e)) e cs | A.Production a e cs <- ps]

        -- Check for duplicate choice constructors.
        conMap = Map.fromListWith (+) [(c,1) | A.Production _ _ css <- ps', A.Choices _ cs <- css, A.Choice _ (Just c) _ _ <- cs]
        chkConDup c = case c of
          Nothing -> []
          Just c  -> case Map.lookup c conMap of Just n -> if n > 1 then [ChoiceConstructorDuplicate] else [] ; _ -> [] 
        ps'' =
          [ A.Production a e
              [ A.Choices a'
                  [A.Choice (tag a'' (chkConDup con)) con asc es
                  | A.Choice a'' con asc es <- cs
                  ]
              | A.Choices a' cs <- css
              ]
          | A.Production a e css <- ps'
          ]

        -- Mark unbound entities within the production bodies.
        ps''' = productions ps''

    in A.Parser a ims ps'''

----------------------------------------------------------------
-- | Other useful functions.

infixPrefixOps :: A.Parser Analysis -> [String]
infixPrefixOps (A.Parser _ _ ps) = 
  nub $ 
       [op | 
         A.Production _ e css <- ps, 
         A.Choices _ cs <- css, 
         c@(A.Choice _ _ _ [A.Terminal (A.Explicit op), A.NonTerminal _ nt]) <- cs,
         ChoiceRecursivePrefix `elem` tags c
       ]
    ++ [op | 
         A.Production _ e css <- ps, 
         A.Choices _ cs <- css, 
         c@(A.Choice _ _ _ [A.NonTerminal _ nt1, A.Terminal (A.Explicit op), A.NonTerminal _ nt2]) <- cs,
         ChoiceRecursiveInfix `elem` tags c
       ]

allOps :: A.Parser Analysis -> [String]
allOps (p@(A.Parser _ _ ps)) = 
  nub $ infixPrefixOps p
    ++  [s | 
          A.Production _ _ css <- ps, 
          A.Choices _ cs <- css, 
          A.Choice _ _ _ es <- cs,
          A.Terminal (A.Explicit s) <- es,
          A.isOp s
        ]

--eof
