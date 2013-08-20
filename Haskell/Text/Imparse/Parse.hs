----------------------------------------------------------------
--
-- | Imparse
--   Cross-platform and -language parser generator.
--
-- @Text\/Imparse\/Parse.hs@
--
--   Parser for Imparse parser specification concrete syntax.
--

----------------------------------------------------------------
--

module Text.Imparse.Parse (parseParser)
  where

import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (catMaybes)
import Data.List (nub, findIndex, isPrefixOf)
import Data.List.Split (splitOn, splitWhen)
import Data.Text (unpack, strip, pack)

import qualified StaticAnalysis.All as A

import Text.Imparse.AbstractSyntax

----------------------------------------------------------------
-- | Exported functions.

parseParser :: A.Analysis a => String -> Either String (Parser a)
parseParser s = 
  let blocks = splitOn "\n\n" (trim s)
  in Right $ Parser A.unanalyzed [] $ catMaybes [pProductionOrDelimiters (trim b) | b <- blocks]

----------------------------------------------------------------
-- | Parsing functions.

pProductionOrDelimiters :: A.Analysis a => String -> Maybe (Production a)
pProductionOrDelimiters s = case splitOn "\n" (noEmptyLines s) of
  line:lines -> 
    case nonemp $ splitOn " " line of
      [entity, "::="] -> 
        Just $ 
          Production A.unanalyzed entity $
            map (Choices A.unanalyzed) $
              map catMaybes $
                splitWhen (\c -> case c of Nothing -> True; _ -> False) $ 
                  [pChoice s | s <- lines, trim s /= ""]
      _ -> Nothing
  _ -> Nothing

pChoice :: A.Analysis a => String -> Maybe (Choice a)
pChoice s = case nonemp $ splitOn " " (trim s) of
  "|":es   -> Just $ Choice A.unanalyzed Nothing AssocNone [pElement e | e <- es, e /= ""]
  "<":es   -> Just $ Choice A.unanalyzed Nothing AssocLeft [pElement e | e <- es, e /= ""]
  ">":es   -> Just $ Choice A.unanalyzed Nothing AssocRight [pElement e | e <- es, e /= ""]
  "~":es   -> Just $ Choice A.unanalyzed Nothing AssocFlat [pElement e | e <- es, e /= ""]
  c:"|":es -> Just $ Choice A.unanalyzed (Just c) AssocNone [pElement e | e <- es, e /= ""]
  c:"<":es -> Just $ Choice A.unanalyzed (Just c) AssocLeft [pElement e | e <- es, e /= ""]
  c:">":es -> Just $ Choice A.unanalyzed (Just c) AssocRight [pElement e | e <- es, e /= ""]
  c:"~":es -> Just $ Choice A.unanalyzed (Just c) AssocFlat [pElement e | e <- es, e /= ""]
  ["^"]      -> Nothing
  _          -> Nothing

pElement :: A.Analysis a => String -> Element a
pElement t = case t of
  '`':'`':s -> Terminal $ Explicit $ '`':s
  "`$"      -> Terminal $ StringLiteral
  "`#"      -> Terminal $ NaturalLiteral
  "`#.#"    -> Terminal $ DecimalLiteral
  "`id"     -> Terminal $ Identifier
  "`var"    -> Terminal $ Identifier
  "`con"    -> Terminal $ Constructor
  "`flag"   -> Terminal $ Flag
  '`':s     -> pNonTerminal s
  _         -> Terminal $ Explicit t

pNonTerminal :: A.Analysis a => String -> Element a
pNonTerminal s =
  if length s >= 1 && isAlpha (head s) && and (map isAlphaNum s) then 
    NonTerminal A.unanalyzed s
  else if length s <= 2 then
    Error $ "`" ++ s
  else if ends "{" "}" s then
    Terminal $ RegExp $ tail $ init s
  else if ends ">>" "<<" s then
    Indented True $ pNonTerminal (drop 2 $ init $ init s)
  else if ends ">" "<" s then
    Indented False $ pNonTerminal (tail $ init s)
  else if ends "[" "]" s then
    let s' = tail $ init s
    in case findIndex (=='/') s' of
         Nothing -> Many (pNonTerminal s') Nothing
         Just i  ->
           let nt = take i s'
               sep = drop (i+1) s'
           in Many (pNonTerminal nt) (Just sep)
  else if ends "(" ")" s then
    May $ pNonTerminal (tail $ init s)
  else
    Error $ "`" ++ s

----------------------------------------------------------------
-- Helpful auxiliary functions.

nonemp :: [String] -> [String]
nonemp = filter ((/=) "")

trim :: String -> String
trim = unpack.strip.pack

ends :: String -> String -> String -> Bool
ends p s t = isPrefixOf p t && isPrefixOf (reverse s) (reverse t)

noEmptyLines :: String -> String
noEmptyLines s = case s of
  '\n':s' ->
    case findIndex (=='\n') s' of
      Nothing -> s
      Just i  -> 
        if nub (take i s') == [' '] then
          "\n\n" ++ noEmptyLines (drop (i+1) s')
        else
          "\n" ++ (noEmptyLines s')
  c   :s' -> c : noEmptyLines s'
  ""      -> ""

--eof
