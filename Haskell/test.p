

Top ::=
  Top | `Stmt

# Test
  # Test.
    # Test.

Stmt ::=
     Return | return `Exp
   Continue | continue
      Break | break
            ^  
   Function | `> function
            | `> break

Stmt ::=
     Return | return
