----------------------------------------------------------------
--
-- Ascetic
-- 
-- Text/Ascetic/HTML.hs
--   Wrappers for building HTML file represented using the
--   Ascetic data structure.

----------------------------------------------------------------
-- 

module Text.Ascetic.HTML
  where

import Data.String.Utils (join)

import qualified Text.Ascetic as A

----------------------------------------------------------------
-- Data structures specific to HTML files.

type Class = String
type Selector = String
type PseudoClass = Maybe String
type Property = String
type Value = String
type DeclarationBlock = [(Property, Value)]
data CSS = CSS [([Selector], PseudoClass, DeclarationBlock)]

type HTML = A.Ascetic

class ToHTML a where
  html :: a -> HTML

----------------------------------------------------------------
-- Combinators for assembling HTML files.

file :: HTML -> HTML -> HTML
file head body = A.E "html" [head, body]

head :: [HTML] -> HTML
head hs = A.E "head" hs

meta_ :: [(A.Attribute, A.Value)] -> HTML
meta_ avs = A.A "meta" avs []

style :: CSS -> HTML
style (CSS dbs) = 
  A.E "style" $ [A.C $ "\n" ++ join "\n\n"
    [ (join ", " ss) ++ (maybe "" ((++) ":") pc) ++ " {\n  " 
        ++ join "\n  " [p ++ ": " ++ v ++ ";" | (p,v) <- pvs] 
        ++ "\n}" 
    | (ss, pc, pvs) <- dbs] ++ "\n"
  ]

script :: String -> HTML
script src = A.E "script" [A.C src]

script_ :: [(A.Attribute, A.Value)] -> String -> HTML
script_ avs src = A.A "script" avs [A.C src]

body :: [HTML] -> HTML
body hs = A.E "body" hs

div :: [HTML] -> HTML
div hs = A.E "div" hs

div_ :: [(A.Attribute, A.Value)] -> [HTML] -> HTML
div_ avs hs = A.A "div" avs hs

span :: [HTML] -> HTML
span hs = A.E "span" hs

span_ :: [(A.Attribute, A.Value)] -> [HTML] -> HTML
span_ avs hs = A.A "span" avs hs

content :: String -> HTML
content s = A.C s

td :: HTML -> HTML
td h = A.E "td" [h]

tr :: [HTML] -> HTML
tr hs = A.E "tr" hs

table :: [HTML] -> HTML
table hs = A.E "table" hs

conc :: [HTML] -> HTML
conc hs = A.L hs

bold :: HTML -> HTML
bold h = A.E "b" [h]

----------------------------------------------------------------
-- Other useful functions.

--eof
