#######################################################
##
## ConvGrammar.py
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
  Production('Grammar', [\
    Choices([\
      Choice('Grammar', AssocNone(), [Many([Nonterminal('Production')])]),\
      ]),\
    ]),\

  # Production Identifier
  Production('Production', [\
    Choices([\
      Choice('Production', AssocNone(), [\
        RegExpr('/[a-z][A-Za-z]*/'), Terminal('::='), Many([Nonterminal('Choice')]),\
        ]),\
      ])\
    ]),\

  # Choice Identifier
  Production('Choice', [\
    Choices([\
      Choice('Choice', AssocNone(), [\
        RegExpr('/[A-Z][A-Za-z]*/'), Terminal('|'), Many([Nonterminal('Expression')])\
        ]),\
      ]),\
    ]),\

  # Operation
  Production('Expression', [\
    Choices([\
      # May
      Choice('May', AssocNone(), [\
        Terminal('['), Many([Nonterminal('Expression')]), Terminal(']')\
        ]),\
      # MayMany
      Choice('MayMany', AssocNone(), [Nonterminal('Expression'), Terminal('*')]),\
      Choice('MayMany', AssocNone(), [\
        Terminal('{'), Many([Nonterminal('Expression')]), Terminal('}')\
        ]),\
      # Many
      Choice('Many', AssocNone(), [Nonterminal('Expression'), Terminal('+')]),\
      ]),\

    Choices([\
      # Terminal
      Choice('Terminal', AssocNone(), [\
        RegExpr('/([^\s`][^\s]+)/'),\
        ]),\
      # Nonterminals
      Choice('Nonterminal', AssocNone(), [\
        RegExpr('/<([A-Z][A-Za-z0-9]*)>/'),\
        ]),\
      # Regular Expression 
      Choice('RegExpr', AssocNone(), [RegExpr('/\$([^\s]*)\$/')]),\
      # Empty String
      Choice('Empty String', AssocNone(), [Terminal('\"\"')]),\
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
