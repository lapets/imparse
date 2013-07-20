----------------------------------------------------------------
--
-- Imparse
--
-- Text/Imparse/Parse.hs
--   Parser for Imparse parser specification concrete syntax.
--

----------------------------------------------------------------
-- 

module Text.Imparse.Parse (parseParser)
  where

import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import Data.Text (unpack, strip, pack)
  
import Text.Imparse.AbstractSyntax
import Text.Imparse.Analysis

----------------------------------------------------------------
-- Exported functions.

parseParser :: String -> Either String (Parser Analysis)
parseParser s = 
  let blocks = splitOn "\n\n" (trim s)
  in Right $ Parser Unanalyzed [] $ catMaybes [parseProductionOrDelimiters (trim b) | b <- blocks]

----------------------------------------------------------------
-- Parsing functions.

parseProductionOrDelimiters :: String -> Maybe (Production Analysis)
parseProductionOrDelimiters s =
  case splitOn "\n" s of
    (line:lines) -> 
      case splitOn " " line of
        [entity, "::="] -> 
          Just $ 
            Production Unanalyzed entity $
              map (Choices Unanalyzed) $ 
                splitOn [PrecedenceSeparator Unanalyzed] $ 
                  catMaybes [parseChoice s | s <- lines, trim s /= ""]
        _ -> Nothing
    _ -> Nothing

parseChoice :: String -> Maybe (Choice Analysis)
parseChoice s =
  case filter ((/=) "") $ splitOn " " (trim s) of
    ("|":es)   -> Just $ Choice Unanalyzed Nothing AssocNone [parseElement e | e <- es, e /= ""]
    ("<":es)   -> Just $ Choice Unanalyzed Nothing AssocLeft [parseElement e | e <- es, e /= ""]
    (">":es)   -> Just $ Choice Unanalyzed Nothing AssocRight [parseElement e | e <- es, e /= ""]
    ("~":es)   -> Just $ Choice Unanalyzed Nothing AssocFlat [parseElement e | e <- es, e /= ""]
    (c:"|":es) -> Just $ Choice Unanalyzed (Just c) AssocNone [parseElement e | e <- es, e /= ""]
    (c:"<":es) -> Just $ Choice Unanalyzed (Just c) AssocLeft [parseElement e | e <- es, e /= ""]
    (c:">":es) -> Just $ Choice Unanalyzed (Just c) AssocRight [parseElement e | e <- es, e /= ""]
    (c:"~":es) -> Just $ Choice Unanalyzed (Just c) AssocFlat [parseElement e | e <- es, e /= ""]
    ["^"]      -> Just $ PrecedenceSeparator Unanalyzed
    _          -> Nothing

parseElement :: String -> Element Analysis
parseElement t =
  case t of
    '`':'`':s -> Terminal $ '`':s
    "`_"      -> NewLine
    "`$"      -> StringLiteral
    "`#"      -> NaturalLiteral
    "`#.#"    -> DecimalLiteral
    '`':'>':'[':s -> 
      if length s > 1 && (reverse s)!!1 == ']' && (reverse s)!!0 == '<' then
        case splitOn "/" (take (length s - 2) s) of
          [s,n,sep] -> Many (NonTerminal Unanalyzed s) (read n) (Just sep)
          [s,n]     -> Many (NonTerminal Unanalyzed s) (read n) Nothing
          _ -> ErrElement t
      else
        ErrElement t
    '`':'>':s -> 
      if length s > 1 && (reverse s)!!0 == '<' then
        Indented (NonTerminal Unanalyzed (take (length s - 1) s))
      else
        ErrElement t
    '`':'[':s -> 
      if length s > 1 && (reverse s)!!0 == ']' then
        case splitOn "/" (take (length s - 1) s) of
          [s,n,sep] -> Many (NonTerminal Unanalyzed s) (read n) (Just sep)
          [s,n]     -> Many (NonTerminal Unanalyzed s) (read n) Nothing
          _ -> ErrElement t
      else
        ErrElement t
    '`':'{':r -> 
      if length r > 1 && (reverse r)!!0 == '}' then
        RegExp $ take (length r - 1) r
      else
        ErrElement t
    '`':s     -> if and $ map isAlphaNum s then NonTerminal Unanalyzed s else ErrElement t
    _         -> Terminal t

----------------------------------------------------------------
-- Helpful auxiliary functions.

trim :: String -> String
trim = unpack.strip.pack

--eof
