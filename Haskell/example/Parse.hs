-- This module generated automatically by imparse.

module Parse
  where

import AbstractSyntax

----------------------------------------------------------------
-- Parser to convert concrete syntax to abstract syntax.

import Text.Parsec
import qualified Text.Parsec.Indent as PI (runIndent, checkIndent, withPos, indented, block)
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Expr as PE
import qualified Text.ParserCombinators.Parsec.Language as PL
import qualified Text.ParserCombinators.Parsec.Prim as Prim

import Control.Monad.Trans.State.Lazy (StateT)
import Data.Functor.Identity (Identity)

----------------------------------------------------------------
-- Parsing functions to export.

parseString :: String -> Either ParseError Top
parseString s = PI.runIndent "" $ runParserT root () "" s

----------------------------------------------------------------
-- Parser state.

type ParseState = StateT SourcePos Identity
type ParseFor a = ParsecT [Char] () ParseState a

----------------------------------------------------------------
-- Parsec-specific configuration definitions and synonyms.

langDef :: PL.GenLanguageDef String () ParseState
langDef = PL.javaStyle
  { PL.identStart        = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijkmlnopqrstuvwxyz_" -- Only lowercase.
  , PL.identLetter       = alphaNum <|> oneOf "_'"
  , PL.opStart           = PL.opLetter langDef
  , PL.opLetter          = oneOf "+"
  , PL.reservedOpNames   = ["+"]
  , PL.reservedNames     = ["host",":","chk","decl","skip","term","test"]
  , PL.commentLine       = "#"
  }

lang :: PT.GenTokenParser [Char] () ParseState
lang = PT.makeTokenParser langDef

whiteSpace = PT.whiteSpace lang
symbol     = PT.symbol lang
rO         = PT.reservedOp lang
res        = PT.reserved lang
identifier = PT.identifier lang
natural    = PT.natural lang

binary name f assoc = PE.Infix (do{PT.reservedOp lang name; return f}) assoc
prefix name f       = PE.Prefix (do{PT.reservedOp lang name; return f})

con :: ParseFor String
con = do { c <- oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ; cs <- option "" identifier ; return $ c:cs }

flag :: ParseFor String
flag = do { cs <- many1 (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ; return cs }
-- caps = do { cs <- many1 (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ; return cs }

block0 p = PI.withPos $ do { r <- many (PI.checkIndent >> p); return r }
may p = option Nothing (do {x <- p; return $ Just x})
(<?|>) p1 p2 = (try p1) <|> p2

----------------------------------------------------------------
-- Parser definition.

root = do { whiteSpace ; r <- pTop ; eof ; return r }

pTop = do {v0 <- (many1 (pHost)); return $ Top v0}
  
pHost = do {res "host"; v1 <- identifier; v2 <- (may (pChk)); res ":"; v4 <- (PI.indented >> block0 (pDecl)); v5 <- (PI.indented >> PI.block (pStmt)); return $ Host v1 v2 v4 v5}
  
pChk = do {res "chk"; return $ Chk }
  
pDecl = do {res "decl"; return $ Decl }
  
pStmt =
      do {res "skip"; return $ Skip }
  <|> do {res "term"; v1 <- pTerm; return $ Term v1}
  
pTerm = PE.buildExpressionParser [[binary "+" Plus PE.AssocLeft]] (
      do {v0 <- identifier; return $ V v0}
  )
pTest = do {res "test"; return $ Test }
  
--eof