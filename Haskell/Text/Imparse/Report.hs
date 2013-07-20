----------------------------------------------------------------
--
-- Imparse
--
-- Text/Imparse/Report.hs
--   Generation of rich reports from parser definitions.
--

----------------------------------------------------------------
-- 

module Text.Imparse.Report
  where

import Data.String.Utils (join)
import Data.List (nubBy)

import qualified Text.RichReports as R

import Text.Imparse.AbstractSyntax

----------------------------------------------------------------
-- Functions for converting a parser abstract syntax instance
-- into a rich report.

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Parser a) where
  report (Parser _ _ ps) = R.Finalize $ R.Conc [R.report p | p <- ps]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Production a) where
  report (Production a e css) = 
    R.Block [] [] [
      R.Line [] [R.Space],
      R.C R.Variable (R.highlights a) (R.messages a) e, R.Text "::=",
      R.BlockIndent [] [] [
        R.Table [
          R.Intersperse 
            (R.Row [R.Field (R.Conc []), R.Field (R.Text "^"), R.Field (R.Conc [])]) 
            [R.report cs | cs <- css]
        ]
      ]
    ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Choices a) where
  report (Choices a cs) = R.Conc [R.report c | c <- cs]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Choice a) where
  report (PrecedenceSeparator a) = R.Text "^"
  report (Choice a c asc es) =
    R.Row [
      R.Field (maybe (R.Conc []) R.Text c),
      R.Field (R.Span (R.highlights a) (R.messages a) [R.Text $ show asc]), 
      R.Field (R.Span [] [] [R.Conc [R.report e | e <- es]])
      ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Element a) where
  report r = case r of
    NonTerminal a n -> R.var_ (R.highlights a) (R.messages a) $ "`" ++ n
    Many e n ms     -> 
      R.Span [] [] $ 
           [ R.key "`[", R.report e, R.key "/", R.Text (show n) ] 
        ++ (maybe [] (\s -> [R.key "/", R.lit s]) ms) 
        ++ [R.key "]"]
    Indented e      -> R.Span [] [] [R.key "`>", R.report e, R.key "<"]
    Terminal t      -> R.key t
    NewLine         -> R.key "`_"
    StringLiteral   -> R.key "`$"
    NaturalLiteral  -> R.key "`#"
    DecimalLiteral  -> R.key "`#.#"
    RegExp r        -> R.Span [] [] [R.key "`{", R.Text r, R.key "}"]
    ErrElement s    -> R.err_ [R.HighlightError] [] $ "`!!!(" ++ s ++ ")!!!"

--eof
