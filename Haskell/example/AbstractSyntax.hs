-- This module generated automatically by imparse.

module AbstractSyntax
  where

data Top = 
    Top [Host]
  deriving (Show, Eq)

data Host = 
    Host  String (Maybe Chk)  [Decl] [Stmt]
  deriving (Show, Eq)

data Chk = 
    Chk 
  deriving (Show, Eq)

data Decl = 
    Decl 
  deriving (Show, Eq)

data Stmt = 
    Skip 
  | Term  Term
  deriving (Show, Eq)

data Term = 
    Plus Term  Term
  | V String
  deriving (Show, Eq)

data Test = 
    Test 
  deriving (Show, Eq)


--eof