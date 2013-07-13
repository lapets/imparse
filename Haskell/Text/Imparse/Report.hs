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
            [R.Conc [R.report c | c <- cs] | cs <- css]
        ]
      ]
    ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Choice a) where
  report (PrecedenceSeparator) = R.Text "^"
  report (Choice c a es) =
    R.Row [
      R.Field (maybe (R.Conc []) R.Text c), 
      R.Field (R.Text (show a)), 
      R.Field (R.Span [] [R.Text "testing"] [R.Conc [R.report e | e <- es]])
      ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Element a) where
  report r = case r of
    NonTerminal a n -> R.C R.Variable (R.highlights a) (R.messages a) $ "`" ++ n
    Many e n ms     -> 
      R.Span [] [] $ 
        [R.Text "`[", R.report e, R.Text "/", R.Text (show n)] ++ (maybe [] (\s -> [R.Text $ "/" ++ s]) ms) ++ [R.Text "]"]
    Indented e      -> R.Span [] [] [R.Text "`>", R.report e, R.Text "<"]
    Terminal t      -> R.C R.Keyword [] [] $ t
    NewLine         -> R.C R.Literal [] [] $ "`_"
    StringLiteral   -> R.C R.Literal [] [] $ "`$"
    RegExp r        -> R.Text $ "`{" ++ r ++ "]"
    ErrElement s    -> R.Text $ "`!!!(" ++ s ++ ")!!!"

--eof
