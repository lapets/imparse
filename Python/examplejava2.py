
#######################################################
##
## Examplejava.py
##
## Example Java of a grammar implementation.
##
#######################################################

exec(open("imparse.py").read())

#######################################################

## Program
program = Production('program', [\
  Choices([\
    Choice('Program', AssocNone(), [Nonterminal('main_class'), MayMany(Nonterminal('class_dec'))])
    ])\
  ])


## Main Class
mainClass = Production('main_class', [\
  Choices([\
    Choice('Main Class', AssocNone(), [Terminal('class'), Nonterminal('id'),\
      Terminal('public'), Terminal('static'), Terminal('void'), Terminal('main'),\
      Terminal('('), Terminal('String'), Terminal('['), Terminal(']'), Nonterminal('id'),\
      Terminal('{'), Nonterminal('statement'), Terminal('}'), Terminal('}')\
      ])\
    ])\
  ])

## Class Declaration
classDec = Production('class_dec', [\
  Choices([\
    Choice('Class', AssocNone(), [Terminal('class'), Nonterminal('id'),\
      May([Terminal('extends'), Nonterminal('id')]),\
      Terminal('{'), MayMany(Nonterminal('variable_dec')), MayMany(Nonterminal('method_dec')), Terminal('}')\
      ])\
    ])\
  ])


## Variable Declaration
varDec = Production('variable_dec', [\
  Choices([\
    Choice('Variable', AssocNone(), [Nonterminal('type'), Nonterminal('id'), Terminal(';')]),\
    Choice('Variable', AssocNone(), [Nonterminal('type'), Nonterminal('id'), Terminal(',')])\
    ])\
  ])


## Method Declaration
methodDec = Production('method_dec', [\
  Choices([\
    Choice('Method', AssocNone(), [Terminal('public'), Nonterminal('type'),\
      Nonterminal('id'), Terminal('('), Nonterminal('type'), Nonterminal('id'),
      MayMany(Terminal(','), Nonterminal('type'), Nonterminal('id')),\
      Terminal(')'), Terminal('{'), MayMany(Nonterminal('variable_dec')),\
      MayMany(Nonterminal('statement')), Terminal('return'),\
      Nonterminal('expression'), Terminal(';'), Terminal('}')\
      ])\
    ])\
  ])


## Type
typej = Production('type', [\
  Choices([\
    Choice('IntArray', AssocNone(), [Terminal('int'), Terminal('['), Terminal(']')]),\
    Choice('Boolean', AssocNone(), [Terminal('boolean')]),\
    Choice('Int', AssocNone(), [Terminal('int')]),\
    Choice('Variable', AssocNone(), [Nonterminal('id')]),\
    ])\
  ])


## Statement
statement = Production('statement', [\
  Choices([\
    Choice(None, AssocNone(), [Terminal('{'), MayMany(Nonterminal('statement')), Terminal('}')]),\
    Choice('If', AssocNone(), [Terminal('if'), Terminal('('), Nonterminal('expression'), Terminal(')'),\
      Nonterminal('statement'), Terminal('else'), Nonterminal('statement')\
      ]),\
    Choice('While', AssocNone(), [Terminal('while'), Terminal('('), Nonterminal('expression'), Terminal(')'),\
      Terminal(')'), Nonterminal('statemet')\
      ]),\
    Choice('Print', AssocNone(), [Terminal('System.out.println'), Terminal('('), Nonterminal('expression'),\
      Terminal(')'), Terminal(';')\
      ]),\
    Choice('Variable', AssocNone(), [Nonterminal('id'), Terminal('='), Nonterminal('expression'), Terminal(';')])\
#    Choice('ArrayElem', AssocNone(), [Nonterminal('id'), Terminal('['), Nonterminal('expression'), Terminal(']'),\
#      Terminal('='), Nonterminal('expression'), Terminal(';')\
#      ])\
    ])\
  ])

## Expression
expression = Production('expression', [\
#  Choices([\
#    Choice('Not', AssocNone(), [Terminal('!'), Nonterminal('expression')]),\
#    Choice('And', AssocNone(), [Nonterminal('expression'), Terminal('&&'), Nonterminal('expression')]),\
#    Choice('Or', AssocNone(), [Nonterminal('expression'), Terminal('||'), Nonterminal('expression')]),\
#    Choice('Equals', AssocNone(), [Nonterminal('expression'), Terminal('=='), Nonterminal('expression')]),\
#    Choice('LessThan', AssocNone(), [Nonterminal('expression'), Terminal('<'), Nonterminal('expression')]),\
#    Choice('LessThanOrEqual', AssocNone(), [Nonterminal('expression'), Terminal('<='), Nonterminal('expression')]),\
#    Choice('GreaterThan', AssocNone(), [Nonterminal('expression'), Terminal('>'), Nonterminal('expression')]),\
#    Choice('GreaterThanOrEqual', AssocNone(), [Nonterminal('expression'), Terminal('>='), Nonterminal('expression')]),\
#    Choice('Plus', AssocNone(), [Nonterminal('expression'), Terminal('+'), Nonterminal('expression')]),\
#    Choice('Minus', AssocNone(), [Nonterminal('expression'), Terminal('-'), Nonterminal('expression')]),\
#    Choice('Div', AssocNone(), [Nonterminal('expression'), Terminal('/'), Nonterminal('expression')]),\
#    Choice('Mult', AssocNone(), [Nonterminal('expression'), Terminal('*'), Nonterminal('expression')]),\
#    ]),\
  Choices([\
    Choice('ArrayElem', AssocNone(), [Nonterminal('expression'), Terminal('['), Nonterminal('expression'), Terminal(']')]),\
    Choice('ArrayLength', AssocNone(), [Nonterminal('expression'), Terminal('.'), Terminal('length')]),\
#    Choice('Mutator', AssocNone(), [Nonterminal('expression'), Terminal('.'), Terminal('id'),\
#      Terminal('('), May([Terminal('expression'), MayMany([Terminal(','), Nonterminal('expression')])]),\
#      Terminal(')')]),\
    ]),\
  Choices([\
    Choice('Int', AssocNone(), [RegExpr('/(0|[1-9][0-9]*)/')]),\
#    Choice('True', AssocNone(), [Terminal('true')]),\
#    Choice('False', AssocNone(), [Terminal('false')]),\
#    Choice('This', AssocNone(), [Terminal('this')]),\
    Choice('NewArray', AssocNone(), [Terminal('new'), Terminal('int'), Terminal('['), Nonterminal('expression'), Terminal(']')]),\
    Choice('Object', AssocNone(), [Terminal('new'), Nonterminal('id'), Terminal('('), Terminal(')')]),\
    Choice(None, AssocNone(), [Terminal('('), Nonterminal('expression'), Terminal(')')]),\
    Choice(None, AssocNone(), [Nonterminal('id')]),\
    ])\
  ])


## Identifier
identifier = Production('id', [\
  Choices([\
    Choice('ID', AssocNone(), [RegExpr('/([a-z][A-Za-z]*)/')])\
    ])\
  ])


## Grammar
grammar = Grammar([expression, identifier])

## Launch interactive parser.
interact()

#eof
