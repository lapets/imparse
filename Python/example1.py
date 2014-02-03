
#######################################################
##
## Example1.py
##
## Example 1 of a grammar implementation.
##
#######################################################

exec(open("imparse.py").read())

#######################################################

grammar = Grammar([\
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
       Choice('Plus', AssocNone(), [Nonterminal('numberLeft'), Terminal('+'), Nonterminal('number')]),\
       Choice('Minus', AssocNone(), [Nonterminal('numberLeft'), Terminal('-'), Nonterminal('number')]),\
       Choice('Number', AssocNone(), [RegExpr('/(0|[1-9][0-9]*)/')])\
     ])\
   ]),\
   Production('numberLeft', [\
     Choices([\
       Choice('Number', AssocNone(), [RegExpr('/(0|[1-9][0-9]*)/')])\
     ])\
   ])\
 ])

# Launch interactive parser.
interact()
