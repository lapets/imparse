

Top ::=
  Top | `[Host]

Host ::=
  Host | host `id `(Chk) : `>>([Decl])<< `>>[Stmt]<<

Chk ::=
  Chk | chk

Decl ::=
  Decl | decl

Stmt ::=
  Skip | skip
  Term | term `Term
  

Term ::=
  Plus | `Term + `Term
       ^
     V | `id

Test ::=
  Test | test
