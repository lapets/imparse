####################################################################
##
## imparse.py
##
##   A cross-platform parser library.
##
##   Web:     imparse.org
##   Version: 0.0.0.4
##
##

import re
import pprint
import uxadt as U
_ = U._

####################################################################
## Data type definitions.

eval(U.definition({ 'Grammar': [_]}))

## Under grammar.
eval(U.definition({\
  'Normal': [],\
  'IndentPresentation': [],\
  'IndentRequired': []\
  }))

eval(U.definition({ 'Production': [_, _] }))
eval(U.definition({ 'Choices': [_] }))
eval(U.definition({ 'Choice': [_, _, _] }))

eval(U.definition({\
  'Just': [_],\
  'Nothing': []\
   }))

eval(U.definition({\
  'AssocNone': [],\
  'AssocLeft': [],\
  'AssocRight': [],\
  'AssocFlat': []\
  }))

eval(U.definition({\
  'One': [_],\
  'May': [_],\
  'Many': [_],\
  'MayMany': [_]\
  }))

eval(U.definition({\
  'Nonterminal': [_],\
  'RegExpr': [_],\
  'Terminal': [_]\
  }))

'''
####################################################################
## Functionality
####################################################################
'''

####################################################################
## Tokenizer
def tokenize(ps, s):
  cbs = [cb for p in ps for cb in p.match(Production(_, _), lambda nt,cbs: cbs).end]
  cs = [c for cb in cbs for c in cb.match(Choices(_), lambda cs: cs).end]
  terminals = []
  for c in cs:
    seq = c.match(Choice(_, _, _), lambda l,a,seq: seq).end
    r = tok(seq)
    if len(r) > 0:
      terminals = terminals + [t for t in r if t not in terminals]
  tmp = [t for t in re.split(r'(\s+|'+'|'.join(terminals)+')[\n]*', s)]
  tokens = [t for t in tmp if not (t == None or t.isspace() or t == '')]
#  print('tokens:', tokens)
  return tokens
    
def tok(seq):
  terminals = []
  for x in seq:
    (ty, e) = etype(x)
    if ty == 'Terminal':
      t = re.escape(e)
#      t = re.escape(x.match(Terminal(_), lambda t: t).end)
      if t not in terminals:
        terminals = terminals + [t]
    elif ty in ['May', 'Many', 'MayMany']:
      r = tok(e)
      if len(r) > 0:
        terminals = terminals + [t for t in r not in terminals]
  return terminals


####################################################################
## Parser

def parse(ps, tmp, nt = None, leftFactor = False):
  for p in ps:
    (pnt, cbs) = p.match(Production(_, _), lambda nt,cbs: (nt, cbs)).end
    # If a nonterminal has been provided as input, skip over productions until
    # the nonterminal and production's nonterminal match.
    if pnt != nt:
      if nt is not None:
        continue
    for cb in cbs:
      cs = cb.match(Choices(_), lambda cs: cs).end

      for c in cs:
        (label, seq) = c.match(Choice(_, _, _), lambda l,a,seq: (l,seq)).end
        (ts, tokens) = (0, tmp[0:])
        if len(tmp) == 0 and len(seq) == 0:
          return (label, [])
        r = parseSeq(ps, tokens, seq, label, (nt, pnt), leftFactor)
        if r is not None and r != True:
          return r


def parseSeq(ps, tokens, seq, label = None, nt = None, leftFactor = False):
  pnt = None
  if nt is not None:
    (nt, pnt) = nt

  (ts, es) = (0, [])
  inseq = 0
  for x in seq:
    (ety, expr) = etype(x)

    if ety == 'One':
      seq2 = expr
      for s in seq2:
        r = parseSeq(ps, tokens, s)
        if r is not None:
          (e, tokens) = r
          if e == []:
            ts = ts + 1
            break
          else:
            es = es + [e]
            break
      if r is None: break

    elif ety in ['May', 'Many', 'MayMany']:
      may = True if ety == 'May' or ety == 'MayMany' else False
      seq2 = expr
      r = parseSeq(ps, tokens, seq2)
      if r is not None:
        (e, tokens) = r
        if e == []:
          ts = ts + 1
        else:
          es = es + [e]
      else: # r is None
        if may == True:
          ts = ts + 1
        else: break
      if ety == 'Many' or ety == 'MayMany':
        while r is not None and len(tokens) > 0:
          r = parseSeq(ps, tokens, seq2)
          if r is not None:
            (e, tokens) = r
            if e == []:
              ts = ts + 1
              inseq = inseq + 1
            else:
              es = es + [e]
              inseq = inseq + 1

    # Terminal
    elif ety == 'Terminal':
      if len(tokens) > 0 and tokens[0] == expr:
        tokens = tokens[1:]
        ts = ts + 1
      else: break

    # Regular expression
    elif ety == 'RegExpr':
      if expr[0] == '/' and expr[-1] == '/':
        if len(tokens) > 0 and re.compile(expr[1:-1]).match(tokens[0]):
          es = es + [tokens[0]]
          tokens = tokens[1:]
        else: break

    # Nonterminal
    elif ety == 'Nonterminal':
      if ts + len(es) == 0:
        if expr == nt and leftFactor: # Top nonterminal
          break
        elif expr == pnt:
          r = parse(ps, tokens, expr, True)
        else:
          r = parse(ps, tokens, expr, False)
      else: # Nonterminal is not the first element of the choice
        r = parse(ps, tokens, expr, False)

      if r is not None:
        (e, tokens) = r
        es = es + [e]
        leftFactor = False
      else: break

  if ts + len(es) == len(seq) + inseq:
    if label is None:
      return (es[0] if len(es) == 1 else es, tokens)
#    if label is None and len(es) == 1:
#      return (es[0], tokens)
#    elif label is None:
#      return (es, tokens)
#    else:
#      return ({label:es} if len(es) > 0 else label, tokens)
    return ({label:es} if len(es) > 0 else label, tokens)
  else:
    return None


def parser(grammar, s):
  ps = grammar.match(Grammar(_), lambda ps: ps).end
  if type(s) == str:
    tokens = tokenize(ps, s)
  else:
    tokens = s
  #print(tokens)

  r = parse(ps, tokens)
  if not r is None:
    (ptree, tokens) = r
    #if len(tokens) < 10:
    #  print(tokens)
    #else:
    #  print(tokens[:10])
    #pprint.pprint(ptree)
    
    if len(tokens) == 0:
      pprint.pprint(ptree)
      return ptree
  print('Syntax error occurred, input could not be parsed.')
  return None


####################################################################
## Interactive parser

def interact(u = None):
  print('Interactive parser. Submit \':q\' or \':quit\' to exit.')
  if u is not None:
    grammar = u
  # Interactive loop.
  while True:
    # Prompt the user for a query.
    s = input('> ')
    if s == ':q' or s == ':quit':
      break
  
    # Parse the query.
    r = parser(grammar, s)
    if not r is None:
      pprint.pprint(r)
    else:
      print('Unknown input.')
    print()


####################################################################
## Useful helper function.

def etype(e):
  return e\
    .match(Terminal(_), lambda t: ('Terminal', t))\
    .match(RegExpr(_), lambda r: ('RegExpr', r))\
    .match(Nonterminal(_), lambda nt: ('Nonterminal', nt))\
    .match(One(_), lambda seq: ('One', seq))\
    .match(May(_), lambda seq: ('May', seq))\
    .match(Many(_), lambda seq: ('Many', seq))\
    .match(MayMany(_), lambda seq: ('MayMany', seq))\
    .end


##eof


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

####################################################################
# Reads a .txt file with a grammar conforming to BNF notation
# and transforms it to UxADT.

def bnfToUxadt(bnfFile):
  tokens = bnfTokenize(bnfFile)
  r = parser(grammar, tokens)
  if r is not None:
#    pprint.pprint(r)
    r = toUxadt(r)
    return r
  return None


def bnfTokenize(bnfFile):
  bnf = open(bnfFile)

  termRes = '\\\"[' + reserved + ']\\\"' # Reserved characters as terminals
  termNT = '::=|[^\s' + reserved + ']+' # Terminals and nonterminals

  regex = '|'.join([\
    comment,\
    regExpr,\
    ('[' + reserved + ']'),\
    termRes,\
    termNT,\
    ])

  regex = re.compile(regex)

  tokens = []
  for line in bnf:
    tokens = tokens + regex.findall(line)
  tokens = [t for t in tokens if not (t == None or t.isspace() or t == '')]
  bnf.close()
#  print(tokens)
  return tokens


def toUxadt(ptree):
  # Unwrap
  ps = ptree['Grammar']
  ps2 = []

  for p in ps:
    prodId = p['Production'][0]
    cs = p['Production'][1:]
    cb = []
    for c in cs:
      (label, seq) = ('', [])
      for i in range(len(c['Choice'])):
        if i == 0:
          label = None if c['Choice'][0] == 'None' else c['Choice'][0]
#          label = c['Choice'][0]
#          if label == 'None':
#            label = None
        else:
          es = c['Choice'][i]
          r = toUxadtExpr(es)
          if r is not None:
            seq = seq + r
          else: break
      cb = cb + [Choice(label, AssocNone(), seq)]
    ps2 = ps2 + [Production(prodId, [Choices(cb)])]
  return Grammar(ps2)

def toUxadtExpr(es):
  if type(es) == str: 
    return [Terminal(es)]

  cs = []
  ty = list(es.keys())[0]
  x = es[ty][0]

  if ty == 'Comment':
    while x[0] == '#' or x[0].isspace():
      x = x[1:]
    if x[-1] == '\n':
      x = x[:-1]
    cs = cs + [Comment(x)]
  elif ty == 'Terminal':
    if len(x) == 3 and x[0] == '\"' and x[-1] == '\"':
      x = x[1]
    cs = cs + [Terminal(x)]
  elif ty == 'Empty String':
    cs = cs + [Terminal('\"\"')]
  elif ty == 'Nonterminal':
    cs = cs + [Nonterminal(x[1:])]
  elif ty == 'RegExpr':
    x = '/' + x[1:-1] + '/'
    cs = cs + [RegExpr(x)]
  # One/May/Many/MayMany
  elif ty == 'One':
    cs = cs + [One(toUxadtExpr(x))]
  elif ty == 'May':
    m = []
    for e in es[ty]:
      m = m + toUxadtExpr(e)
    cs = cs + [May(m)]
  elif ty == 'Many':
    m = []
    for e in es[ty]:
      m = m + toUxadtExpr(e)
    cs = cs + [Many(m)]
  elif ty == 'MayMany':
    m = []
    for e in es[ty]:
      m = m + toUxadtExpr(e)
    cs = cs + [MayMany(m)]
  # Group
  elif ty == 'Group':
    for e in es[ty]:
      cs = cs + toUxadtExpr(e)

  return cs


#######################################################
# Writes a UxADT grammar to a file.

def writeUxadt(u, name = 'gen', indent = 2):
  s = uxadtToStr(u, indent)
  f = open(name + '.py', 'w')
  f.write(
'''
##########################################
##
##  UxADT grammar generated by Imparse.
##
##########################################

'''
)
  f.write(name + 'Uxadt = ')
  f.write(s)
  f.close()

def uxadtToStr(u, indent = 2):
  indent = ' ' * indent
  ps = u.match(Grammar(_), lambda ps: ps).end
  st = 'Grammar([\\'
  for p in ps:
    (nt, cbs) = p.match(Production(_, _), lambda nt, cbs: (nt, cbs)).end
    st = st + '\n' + indent + 'Production(\'' + nt + '\', [\\'
    for cb in cbs:
      cs = cb.match(Choices(_), lambda cs: cs).end
      st = st + '\n' + (indent * 2) + 'Choices([\\'
      for c in cs:
        (label, seq) = c.match(Choice(_, _, _), lambda l, a, seq: (l, seq)).end
        if label is None:
          st = st + '\n' + (indent * 3) + 'Choice(None, AssocNone(), [\\'
        else:
          st = st + '\n' + (indent * 3) + 'Choice(\'' + label + '\', AssocNone(), [\\'
        r = uxadtSeqToStr(seq)
        st = st + '\n' + (indent * 4) + r + '\\\n' + (indent * 4) + ']),\\'
      st = st + '\n' + (indent * 3) + ']),\\'
    st = st + '\n' + (indent * 2) + ']),\\'
  st = st + '\n'+ indent + '])'
  return st

def uxadtSeqToStr(seq):
  s = ''
  for x in seq:
    et = etype(x)
    (ty, expr) = et
 
    if ty == 'One':
      r = uxadtSeqToStr(expr)
      s = s + 'One([' + r + ']), '
    elif ty == 'May':
      r = uxadtSeqToStr(expr)
      s = s + 'May([' + r + ']), '
    elif ty == 'Many':
      r = uxadtSeqToStr(expr)
      s = s + 'Many([' + r + ']), '
    elif ty == 'MayMany':
      r = uxadtSeqToStr(expr)
      s = s + 'MayMany([' + r + ']), '
    elif ty == 'Terminal':
      s = s + 'Terminal(\'' + expr + '\'), '
    elif ty == 'RegExpr':
      s = s + 'RegExpr(\'' + expr + '\'), '
    elif ty == 'Nonterminal':
      s = s + 'Nonterminal(\'' + expr + '\'), '
  return s
  

##eof