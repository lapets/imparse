-- This module generated automatically by imparse.

module Report
  where

import qualified Text.RichReports as R

import AbstractSyntax

instance R.ToReport Top where
  report x = case x of
    Top v0 -> R.Span [] [] $ [R.report v0]
    
instance R.ToReport Host where
  report x = case x of
    Host v1 v2 v4 v5 -> R.Span [] [] $ [R.key "host", R.var v1, R.report v2, R.key ":", R.BlockIndent [] [] $ [R.Line [] [R.report vx] | vx <- v4], R.BlockIndent [] [] $ [R.Line [] [R.report vx] | vx <- v5]]
    
instance R.ToReport Chk where
  report x = case x of
    Chk  -> R.Span [] [] $ [R.key "chk"]
    
instance R.ToReport Decl where
  report x = case x of
    Decl  -> R.Span [] [] $ [R.key "decl"]
    
instance R.ToReport Stmt where
  report x = case x of
    Skip  -> R.Span [] [] $ [R.key "skip"]
    Term v1 -> R.Span [] [] $ [R.key "term", R.report v1]
    
instance R.ToReport Term where
  report x = case x of
    Plus v0 v2 -> R.Span [] [] $ [R.report v0, R.key "+", R.report v2]
    V v0 -> R.Span [] [] $ [R.var v0]
    
instance R.ToReport Test where
  report x = case x of
    Test  -> R.Span [] [] $ [R.key "test"]
    

--eof