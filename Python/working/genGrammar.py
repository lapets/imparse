####################################################################
##
## GenGrammar.py
##
## A cross-platform parser library.
##
##    Web:     imparse.org
##    Version: 0.0.0.4
##
##

exec(open('imparse.py').read())

## RELEASE CONTENT BEGINS HERE #####################################

####################################################################
# Definitions.

reserved = '#\$\*\+\(\)\{\}\[\]\|' # Reserved characters
comment = '#[^#]+'
regExpr = '\$[^$]+\$'
production = '[A-Z][A-Za-z0-9]*'
label = '[A-Z][A-Za-z0-9]*'
nonterminal = '`[A-Z][A-Za-z0-9]*'
terminal = '[^A-Z' + reserved + '][^\s' + reserved + ']*'

grammar = Grammar([\
  # Comments
  Production('Comment', [\
    Choices([\
      Choice('Comment', AssocNone(), [\
        RegExpr('/' + comment + '/'),\
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
        RegExpr('/' + production + '/'), Terminal('::='), Many([Nonterminal('Choice')]),\
        ]),\
      ])\
    ]),\

  # Choice Identifier
  Production('Choice', [\
    Choices([\
      Choice('Choice', AssocNone(), [\
        #May([Nonterminal('Comment')]),\
        RegExpr('/' + label + '/'), Terminal('|'), Many([Nonterminal('Expression')])\
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
      # Many
      Choice('Many', AssocNone(), [\
        Nonterminal('Expression'), Terminal('+')\
        ]),\
      # MayMany
      Choice('MayMany', AssocNone(), [\
        Nonterminal('Expression'), Terminal('*'),\
        ]),\
      Choice('MayMany', AssocNone(), [\
        Terminal('{'), Many([Nonterminal('Expression')]), Terminal('}')\
        ]),\
      ]),\

    Choices([\
      # Regular Expression 
      Choice('RegExpr', AssocNone(), [\
        RegExpr('/' + regExpr + '/'),\
        ]),\
      # Nonterminals
      Choice('Nonterminal', AssocNone(), [\
        RegExpr('/' + nonterminal + '/'),\
        ]),\
      # Empty String
      Choice('Empty String', AssocNone(), [\
        Terminal('\"\"'),\
        ]),\
      # Terminal
      Choice('Terminal', AssocNone(), [\
        RegExpr('/\"[' + reserved + ']\"'),\
        ]),\
      Choice('Terminal', AssocNone(), [\
        #RegExpr('/([^A-Z#`$*+\(\)\{\}\[\]\|][^\s#`$*+\(\)\{\}\[\]\|]*)/'),\
#        RegExpr('/([^A-Z#`*+\$\(\)\{\}\[\]\|][^\s*+]*)/'),\
        RegExpr('/' + terminal + '/'),\
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