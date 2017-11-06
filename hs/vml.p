
Top ::=
  Top | `[Host/1]

Host ::=
  Host | host `Variable : `>[Stmt/1]<

Stmt ::=
     Skip | skip
   Assign | `Variable := `Term
   Invoke | `Action
       If | if `Formula : `>[Stmt/1]<
     Loop | loop : `>[Stmt/1]<

Action ::=
  Action | `Variable . `Variable ( `[Constant/0/,] )

Formula ::=
  And | `Formula && `Formula
   Or | `Formula || `Formula
      ^
  Not | ! `Formula
      ^
   Eq | `Term == `Term
  Neq | `Term !=  `Term
   Lt | `Term <  `Term
  Leq | `Term <= `Term
   Gt | `Term >  `Term
  Geq | `Term >= `Term
   In | `Term in `Term

Term ::=
      V | `Variable
      N | `{[0-9]+}
        ^
   Plus | `Term + `Term
  Minus | `Term - `Term
        ^
   Mult | `Term * `Term
    Div | `Term / `Term
        ^
    Pow | `Term ^ `Term
        ^
    Neg | - `Term

Variable ::=
  Variable | `id

Constant ::=
  Constant | `con
