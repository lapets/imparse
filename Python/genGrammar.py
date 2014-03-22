#######################################################
##
## GenGrammar.py
##
## A cross-platform parser library.
##
##    Web:     imparse.org
##    Version: 0.0.0.4
##
##
#######################################################

exec(open('imparse.py').read())

grammar = Grammar([\
  # Comments
  Production('Comment', [\
    Choices([\
      Choice('Comment', AssocNone(), [\
#        Terminal('/*'), Many([Nonterminal('Production')]), Terminal('*/')\
        RegExpr('/#[^#]+/'),\
        ]),\
      ]),\
    ]),\

  # Grammar 
  Production('Grammar', [\
    Choices([\
      Choice('Grammar', AssocNone(), [\
        Many([Nonterminal('Production')]),\
        ]),\
      ]),\
    ]),\

  # Production Identifier
  Production('Production', [\
    Choices([\
      Choice('Production', AssocNone(), [\
        #RegExpr('/[A-Z][A-Za-z0-9]*[ ]::=/'), Many([Nonterminal('Choice')]),\
        May([Nonterminal('Comment')]),\
        RegExpr('/[A-Z][A-Za-z0-9]*/'), Terminal('::='), Many([Nonterminal('Choice')]),\
        ]),\
      ])\
    ]),\

  # Choice Identifier
  Production('Choice', [\
    Choices([\
      Choice('Choice', AssocNone(), [\
        #May([Nonterminal('Comment')]),\
        RegExpr('/[A-Z][A-Za-z]*/'), Terminal('|'), Many([Nonterminal('Expression')])\
        ]),\
      ]),\
    ]),\

  # Expressions 
  Production('Expression', [\
    Choices([\
      # May
      Choice('May', AssocNone(), [\
        Terminal('['), Many([Nonterminal('Expression')]), Terminal(']')\
        ]),\
      # MayMany
      Choice('MayMany', AssocNone(), [\
        Nonterminal('Expression'), Terminal('*'),\
        ]),\
      Choice('MayMany', AssocNone(), [\
        Terminal('{'), Many([Nonterminal('Expression')]), Terminal('}')\
        ]),\
      # Many
      Choice('Many', AssocNone(), [Nonterminal('Expression'), Terminal('+')]),\
      ]),\

    Choices([\
      # Regular Expression 
      Choice('RegExpr', AssocNone(), [\
        RegExpr('/\$[^$]+\$/'),\
        ]),\
      # Nonterminals
      Choice('Nonterminal', AssocNone(), [\
        RegExpr('/`([A-Z][A-Za-z0-9]*)/'),\
        ]),\
      # Empty String
      Choice('Empty String', AssocNone(), [\
        Terminal('\"\"'),\
        ]),\
      # Terminal
      Choice('Terminal', AssocNone(), [\
        RegExpr('/\"([#*+\$\(\)\{\}\[\]\|])\"'),\
#        RegExpr('/\"\*\"'),\
        ]),\
      Choice('Terminal', AssocNone(), [\
        #RegExpr('/([^A-Z#`$*+\(\)\{\}\[\]\|][^\s#`$*+\(\)\{\}\[\]\|]*)/'),\
        RegExpr('/([^A-Z#`*+\$\(\)\{\}\[\]\|][^\s*+]*)/'),\
        ]),\
      ]),\

    Choices([\
      # Group
      Choice('Group', AssocNone(), [\
        Terminal('('), Many([Nonterminal('Expression')]), Terminal(')')\
        ]),\
      ]),\
    ]),\
  ])


##eof
