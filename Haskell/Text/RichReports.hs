----------------------------------------------------------------
--
-- Rich Reports
--
-- RichReports.hs
--   Definitions for the representation and construction of a
--   data structure corresponding to a structured representation
--   of the concrete syntax of a programming language, with
--   annotations corresponding to static analysis results.
--   Includes support for generation of ASCII text, as well as
--   formatted HTML with interactive messages.

----------------------------------------------------------------
--

module Text.RichReports
  where

import Data.List (intersperse)
import Data.String.Utils (join)

import qualified Text.Ascetic.HTML as H

----------------------------------------------------------------
-- Data structures (also used for report construction process)
-- and class.

type Message = Report

data Highlight =
    Unbound
  | Duplicate
  | Error
  | Highlight [H.Class]
  deriving (Eq, Show)

data Category =
    Keyword
  | Literal
  | Constant
  | Variable
  deriving (Eq, Show)

data Report =
    Text String
  | C Category [Highlight] [Message] String
  | Space
  | Lt
  | Gt
  | Conc [Report]
  | Field Report
  | Row [Report]
  | Table [Report]
  | Indent Report
  | Line [String] [Report]
  | LineIfFlat [String] Report
  | Span [Highlight] [Message] [Report]
  | Block [Highlight] [Message] [Report]
  | BlockIndent [Highlight] [Message] [Report]
  | Intersperse Report [Report]
  | Finalize Report
  deriving (Show, Eq)

class ToReport a where
  report :: a -> Report

class ToHighlights a where
  highlights :: a -> [Highlight]

class ToMessages a where
  messages :: a -> [Message]

----------------------------------------------------------------
-- Default class members.

-- None.

----------------------------------------------------------------
-- Generation of an interactive HTML version of the report.

highlight :: Highlight -> [H.Class]
highlight h = case h of
  Unbound -> ["RichReports_Highlight_Unbound"]
  Duplicate -> ["RichReports_Highlight_Duplicate"]
  Error -> ["RichReports_Highlight_Error"]
  Highlight hs -> hs

messageToAttr :: [Message] -> (H.Property, H.Value)
messageToAttr ms = ("onclick","msg(this, [" ++ (join "," ["'" ++ show (H.html m) ++ "'" | m <- ms]) ++ "]);")

instance H.ToHTML Report where
  html r = case r of
    Text s -> H.content s
    C c hs ms s -> 
      H.span_ 
        (    [ ("class", 
                "RichReports_" ++ show c 
                ++ " " ++ (if length ms > 0 then "RichReports_Clickable" else "")
                ++ " " ++ (if length hs > 0 then "RichReports_Highlight" else "")
                ++ " " ++ (join " " (concat (map highlight hs)))
                )
             ]
          ++ ( if length ms > 0 then [messageToAttr ms] else [] )
        )
        [H.content s]
    Space -> H.content "&nbsp;"
    Conc rs -> H.conc [H.html r | r <- rs]
    Field r -> H.td (H.html r)
    Row rs -> H.tr [ H.html r | r <- rs ]
    Table rs -> H.table [ H.html r | r <- rs ]
    Line _ rs -> H.div [H.html r | r <- rs]
    Span hs ms rs ->
      let out = H.span_ [("class", join " " (concat (map highlight hs)))] [H.html r | r <- rs]
      in case ms of
        [] -> out
        ms -> H.span [H.span_ [("class","RichReports_Clickable RichReports_Clickable_Exclamation"), messageToAttr ms] [H.content "!"], out]
    Block _ _ rs -> H.div [H.html r | r <- rs]
    BlockIndent _ _ rs -> H.div_ [("class", "RichReports_BlockIndent")] [H.html r | r <- rs]
    Intersperse r rs -> H.conc $ intersperse (H.html r) [H.html r | r <- rs]
    Finalize r -> 
      H.file 
        (H.head [
          H.meta_ [("http-equiv","Content-type"),("content","text/html;charset=UTF-8")],
          H.style (
            H.CSS [
              ( ["body"], 
                Nothing,
                [ ("font-family", "Courier,Monospace"),
                  ("font-size", "12px")
                ]
              ),
              ( ["table"], 
                Nothing,
                [ ("font-family", "Courier,Monospace"),
                  ("font-size", "12px")
                ]
              ),
              ( ["#RichReports_Message"], 
                Nothing, 
                [ ("background-color","yellow"),
                  ("padding","3px"),
                  ("border","1px solid black"),
                  ("font-family", "Courier,Monospace"),
                  ("font-size", "12px")
                ]
              ),
              ([".RichReports_Clickable"], Nothing, [("cursor","pointer")]),
              ( [".RichReports_Clickable_Exclamation"], 
                Nothing, 
                [("background-color","yellow"), ("border","1px solid black"), ("margin","0px 5px 0px 5px")]
              ),
              ([".RichReports_Clickable"], Just "hover", [("background-color","yellow")]),
              ([".RichReports_BlockIndent"], Nothing, [("margin-left", "10px")]),
              ([".RichReports_Keyword"], Nothing, [("font-weight", "bold"), ("color", "blue")]),
              ([".RichReports_Variable"], Nothing, [("font-style", "italic"), ("color", "green")]),
              ([".RichReports_Literal"], Nothing, [("font-weight", "bold"), ("color", "firebrick")]),
              ([".RichReports_Highlight"], Nothing, [("margin","2px")]),
              ([".RichReports_Highlight_Unbound"], Nothing, [("background-color", "orange")]),
              ([".RichReports_Highlight_Duplicate"], Nothing, [("background-color", "yellow")]),
              ([".RichReports_Highlight_Error"], Nothing, [("background-color", "lightpink")])
            ]
          ),
          H.script_ [("type","text/javascript"), ("src","http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js")] "",
          H.script $
            "function msg (obj, msgs) {"
              ++ "var html = '';"
              ++ "for (var i = 0; i < msgs.length; i++) html += '<div class=\"RichReports_MessagePortion\">' + msgs[i] + '</div>';"
              ++ "document.getElementById('RichReports_Message').innerHTML = html;"
              ++ "document.getElementById('RichReports_Message').style.display = 'inline-block';"
              ++ "var top = $(obj).offset().top;"
              ++ "var left = $(obj).offset().left;"
              ++ "$('#RichReports_Message').offset({top:top + 15, left:left + 15});"
              ++ "}"
        ])
        (H.body [
          H.html r,
          H.div_ [("id","RichReports_Message"), ("style","display:none;"), ("onclick", "this.style.display='none';")] [H.content ""]
        ])
    
    
    
    _ -> H.content ""

--eof
