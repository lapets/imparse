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
import Data.List.Split (splitOn, splitWhen)
import Data.Text (unpack, strip, pack)

import qualified StaticAnalysis.All as A

import Text.Imparse.AbstractSyntax

----------------------------------------------------------------
-- Exported functions.

parseParser :: A.Analysis a => String -> Either String (Parser a)
parseParser s = 
  let blocks = splitOn "\n\n" (trim s)
  in Right $ Parser A.unanalyzed [] $ catMaybes [parseProductionOrDelimiters (trim b) | b <- blocks]

----------------------------------------------------------------
-- Parsing functions.

parseProductionOrDelimiters :: A.Analysis a => String -> Maybe (Production a)
parseProductionOrDelimiters s =
  case splitOn "\n" s of
    (line:lines) -> 
      case splitOn " " line of
        [entity, "::="] -> 
          Just $ 
            Production A.unanalyzed entity $
              map (Choices A.unanalyzed) $
                map catMaybes $
                  splitWhen (\c -> case c of Nothing -> True; _ -> False) $ 
                    [parseChoice s | s <- lines, trim s /= ""]
        _ -> Nothing
    _ -> Nothing

parseChoice :: A.Analysis a => String -> Maybe (Choice a)
parseChoice s =
  case filter ((/=) "") $ splitOn " " (trim s) of
    ("|":es)   -> Just $ Choice A.unanalyzed Nothing AssocNone [parseElement e | e <- es, e /= ""]
    ("<":es)   -> Just $ Choice A.unanalyzed Nothing AssocLeft [parseElement e | e <- es, e /= ""]
    (">":es)   -> Just $ Choice A.unanalyzed Nothing AssocRight [parseElement e | e <- es, e /= ""]
    ("~":es)   -> Just $ Choice A.unanalyzed Nothing AssocFlat [parseElement e | e <- es, e /= ""]
    (c:"|":es) -> Just $ Choice A.unanalyzed (Just c) AssocNone [parseElement e | e <- es, e /= ""]
    (c:"<":es) -> Just $ Choice A.unanalyzed (Just c) AssocLeft [parseElement e | e <- es, e /= ""]
    (c:">":es) -> Just $ Choice A.unanalyzed (Just c) AssocRight [parseElement e | e <- es, e /= ""]
    (c:"~":es) -> Just $ Choice A.unanalyzed (Just c) AssocFlat [parseElement e | e <- es, e /= ""]
    ["^"]      -> Nothing
    _          -> Nothing

parseElement :: A.Analysis a => String -> Element a
parseElement t =
  case t of
    '`':'`':s -> Terminal $ Explicit $ '`':s
    "`_"      -> Terminal $ NewLine
    "`$"      -> Terminal $ StringLiteral
    "`#"      -> Terminal $ NaturalLiteral
    "`#.#"    -> Terminal $ DecimalLiteral
    "`id"     -> Terminal $ Identifier
    "`con"    -> Terminal $ Constructor
    "`flag"   -> Terminal $ Flag
    '`':'>':'[':s -> 
      if length s > 1 && (reverse s)!!1 == ']' && (reverse s)!!0 == '<' then
        case splitOn "/" (take (length s - 2) s) of
          [s,n,sep] -> Many (NonTerminal A.unanalyzed s) (read n) (Just sep)
          [s,n]     -> Many (NonTerminal A.unanalyzed s) (read n) Nothing
          _ -> Error t
      else
        Error t
    '`':'>':s -> 
      if length s > 1 && (reverse s)!!0 == '<' then
        Indented (NonTerminal A.unanalyzed (take (length s - 1) s))
      else
        Error t
    '`':'[':s -> 
      if length s > 1 && (reverse s)!!0 == ']' then
        case splitOn "/" (take (length s - 1) s) of
          [s,n,sep] -> Many (NonTerminal A.unanalyzed s) (read n) (Just sep)
          [s,n]     -> Many (NonTerminal A.unanalyzed s) (read n) Nothing
          _ -> Error t
      else
        Error t
    '`':'{':r -> 
      if length r > 1 && (reverse r)!!0 == '}' then
        Terminal $ RegExp $ take (length r - 1) r
      else
        Error t
    '`':s     -> if and $ map isAlphaNum s then NonTerminal A.unanalyzed s else Error t
    _         -> Terminal $ Explicit t

----------------------------------------------------------------
-- Helpful auxiliary functions.

trim :: String -> String
trim = unpack.strip.pack

--eof
