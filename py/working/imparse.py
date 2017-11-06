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

eval(U.define({ 'Grammar': [_]}))

## Under grammar.
eval(U.define({\
  'Normal': [],\
  'IndentPresentation': [],\
  'IndentRequired': []\
  }))

eval(U.define({ 'Production': [_, _] }))
eval(U.define({ 'Choices': [_] }))
eval(U.define({ 'Choice': [_, _, _] }))

eval(U.define({\
  'Just': [_],\
  'Nothing': []\
   }))

eval(U.define({\
  'AssocNone': [],\
  'AssocLeft': [],\
  'AssocRight': [],\
  'AssocFlat': []\
  }))

eval(U.define({\
  'One': [_],\
  'May': [_],\
  'Many': [_],\
  'MayMany': [_]\
  }))

eval(U.define({\
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
    if x < Terminal(_):
      (e,) = x
      t = re.escape(e)
#      t = re.escape(x.match(Terminal(_), lambda t: t).end)
      if t not in terminals:
        terminals = terminals + [t]
    elif x < May(_) or x < Many(_) or x < MayMany(_):
      (e,) = x
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
    if x < One(_):
      (expr,) = x
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

    elif x < May(_) or x < Many(_) or x < MayMany(_):
      (expr,) = x
      may = x < May(_) or x < MayMany(_)
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
      if x < Many(_) or x < MayMany(_):
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
    elif x < Terminal(_):
      (expr,) = x
      if len(tokens) > 0 and tokens[0] == expr:
        tokens = tokens[1:]
        ts = ts + 1
      else: break

    # Regular expression
    elif x < RegExpr(_):
      (expr,) = x
      if expr[0] == '/' and expr[-1] == '/':
        if len(tokens) > 0 and re.compile(expr[1:-1]).match(tokens[0]):
          es = es + [tokens[0]]
          tokens = tokens[1:]
        else: break

    # Nonterminal
    elif  x < Nonterminal(_):
      (expr,) = x
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

##eof
