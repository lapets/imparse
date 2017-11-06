----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Text\/Imparse\/Report.hs@
--
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
-- | Functions for converting a parser abstract syntax instance
--   into a rich report.

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Parser a) where
  report (Parser _ _ ps) = R.Finalize $ R.Conc [R.report p | p <- ps]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Production a) where
  report (Production a e css) = 
    R.Block [] [] [
      R.Line [] [R.Space],
      R.C R.Variable (R.highlights a) (R.messages a) e, R.Text "::=",
      R.BlockIndent [] [] [
        R.Table [ R.report cs | cs <- css ]
      ]
    ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Choices a) where
  report (Choices a cs) = 
    R.Conc $ 
      [R.report c | c <- cs] ++ 
      [R.Row [
        R.Field (R.Conc []), 
        R.Field (R.Atom (R.highlights a) (R.messages a) [R.Text "^"]), 
        R.Field (R.Conc [])]
      ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Choice a) where
  report (Choice a c asc es) =
    R.Row [
      R.Field (maybe (R.Conc []) R.Text c),
      R.Field (R.Atom (R.highlights a) (R.messages a) [R.Text $ show asc]), 
      R.Field (R.Span [] [] [R.Conc [R.report e | e <- es]])
      ]

instance (R.ToHighlights a, R.ToMessages a) => R.ToReport (Element a) where
  report (Terminal t) = R.report t
  report (Error s)    = R.err_ [R.HighlightError] [] $ "`!!!_" ++ s ++ "_!!!"
  report r = 
    let rec d r = 
          let bq = if d > 0 then "" else "`"
          in case r of
               NonTerminal a nt -> R.var_ (R.highlights a) (R.messages a) $ bq ++ nt
               Many e ms -> 
                 R.Span [] [] $ 
                     [ R.key (bq ++ "["), rec (d+1) e ]
                   ++ (maybe [] (\s -> [R.key "/", R.lit s]) ms) 
                   ++ [R.key "]"]
               May e -> R.Span [] [] $ [ R.key (bq ++ "("), rec (d+1) e ] ++ [R.key ")"]
               Indented w e ->
                 if w then
                   R.Span [] [] [R.key (bq ++ ">"), R.report e, R.key "<"]
                 else
                   R.Span [] [] [R.key (bq ++ ">>"), R.report e, R.key "<<"]
    in rec 0 r

instance R.ToReport Terminal where
  report t = case t of
    Explicit s     -> R.lit s
    StringLiteral  -> R.key "`$"
    NaturalLiteral -> R.key "`#"
    DecimalLiteral -> R.key "`#.#"
    Identifier     -> R.key "`var"
    Constructor    -> R.key "`con"
    Flag           -> R.key "`flag"
    RegExp r       -> R.Span [] [] [R.key "`{", R.Text r, R.key "}"]

--eof
