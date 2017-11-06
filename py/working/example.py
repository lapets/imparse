
#######################################################
##
## Example.py
##
## Example of a grammar implementation.
##
#######################################################

exec(open("imparse.py").read())

#######################################################

example = Grammar([\
   Production('formula', [\
     Choices([\
       Choice('And', AssocNone(), [Nonterminal('formula'), Terminal('and'), Nonterminal('formula')]),\
       Choice('Or', AssocNone(), [Nonterminal('formula'), Terminal('or'), Nonterminal('formula')])\
     ]),\
     Choices([\
       Choice('Equal', AssocNone(), [Nonterminal('number'), Terminal('=='), Nonterminal('number')]),\
       Choice('True', AssocNone(), [Terminal('true')]),\
       Choice('False', AssocNone(), [Terminal('false')]),\
       Choice('Not', AssocNone(), [Terminal('not'), Nonterminal('formula')]),\
     ])\
   ]),\
   Production('number', [\
     Choices([\
       Choice('Plus', AssocNone(), [Nonterminal('number'), Terminal('+'), Nonterminal('number')]),\
       Choice('Minus', AssocNone(), [Nonterminal('number'), Terminal('-'), Nonterminal('number')]),\
       Choice('Number', AssocNone(), [RegExpr('/(0|[1-9][0-9]*)/')])\
     ])\
   ]),\
 ])

## Launch interactive parser.
interact(example)

#eof
