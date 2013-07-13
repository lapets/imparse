module AbstractSyntax
  where

data Top = 
    Top Stmt 
  deriving (Show, Eq)

data Stmt = 
    Return Exp 
  | Continue 
  | Break 
  | Function 
  | C0 
  deriving (Show, Eq)

data Stmt = 
    Return 
  deriving (Show, Eq)


--eof