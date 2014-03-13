#######################################################
##
## Imparse.py
##
## A cross-platform parser library.
##
##    Web:     imparse.org
##    Version: 0.0.0.4
##
##
#######################################################

import re
import pprint
import uxadt as U

exec(open('uxadt.py').read())
exec(open('definitions.py').read())

#######################################################
## Functionality

## Tokenizer
def tokenize(ps, s):
  terminals = []
  cbs = [cb for p in ps for cb in p.match(Production(_, _), lambda nt,cbs: cbs).end]
  cs = [c for cb in cbs for c in cb.match(Choices(_), lambda cs: cs).end]
  for c in cs:
    seq = c.match(Choice(_, _, _), lambda l,a,seq: seq).end
    for x in seq:
      if etype(x) == 'Terminal':
        t = re.escape(x.match(Terminal(_), lambda t: t).end)
        if t not in terminals:
          terminals = terminals + [t]

  tmp = [t for t in re.split(r'(\s+|'+'|'.join(terminals)+')[\n]*', s)]
  tokens = [t for t in tmp if not (t == None or t.isspace() or t == '')]
  return tokens


def parse(ps, tmp, nt = None, leftFactor = False):
  for p in ps:
    pnt = p.match(Production(_, _), lambda nt,cbs: nt).end
    # If a nonterminal has been provided as input, skip over productions until
    # the nonterminal and production's nonterminal match.
    if pnt != nt:
      if nt is not None:
        continue
    cbs = p.match(Production(_, _), lambda nt,cbs: cbs).end
    for cb in cbs:
      cs = cb.match(Choices(_), lambda cs: cs).end

      for c in cs:
        (label, seq) = c.match(Choice(_, _, _), lambda l,a,seq: (l,seq)).end
        (ts, es, tokens) = (0, [], tmp[0:])
        if len(tmp) == 0:
          if len(seq) == 0:
            return (label, [])
        r = parExpr(ps, tokens, seq, label, (nt, pnt), leftFactor)
        if r is not None and r != True:
#          print('r in parse:', r)
          (e, tokens) = r
          es = es + [e]
          return r


def parExpr(ps, tokens, seq, label = None, nt = None, leftFactor = False):
  (ts, es, pnt) = (0, [], None)
  if nt is not None:
    (nt, pnt) = nt
#    print('\nnew call -- NT:', nt, '\tPNT:', pnt)
#  else:
#    print('\nnew call -- NT:', nt)

  inseq = 0
  for x in seq:
    (ety, expr) = etype(x)
#    print('Expr:', ety, '\t', expr)
#    print('Tokens:', tokens)

    if ety in ['One', 'May', 'Many', 'MayMany']:
      may = True if ety == 'May' or ety == 'MayMany' else False
      seq2 = expr
      r = parExpr(ps, tokens, seq2)
#      print('R in May/Many/etc:\t', r, '\n')
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
#        print('INSIDE IF STATEMENT~~~~~~~~~~~~~~~~~')
        while r is not None:
#          print('Loop tokens', tokens)
          r = parExpr(ps, tokens, seq2)
#          print('R', r)
          if r is not None:
            (e, tokens) = r
            if e == []:
              ts = ts + 1
              inseq = inseq + 1
            else:
              es = es + [e]
              inseq = inseq + 1
#          print('End loop tokens', tokens)

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
#          print('expr = pnt', expr, '==', pnt)
          r = parse(ps, tokens, expr, True)
        else:
#          print('expr != pnt', expr, '!=', pnt)
          r = parse(ps, tokens, expr, False)
      else: # Nonterminal is not the first element of the choice
#        print('else', expr, tokens)
        r = parse(ps, tokens, expr, False)

      if r is not None:
        (e, tokens) = r
        es = es + [e]
        leftFactor = False
      else: break

  if ts + len(es) == len(seq) + inseq:
    if label is None and len(es) == 1:
#      print('Return 1:',  es[0], tokens)
      return (es[0], tokens)
    elif label is None:
#      print('Return 2:', es, tokens)
      return (es, tokens)
    else:
#      print('Return 3: Dict')
      return ({label:es} if len(es) > 0 else label, tokens)
  else:
#    print('Return 4: None')
    return None


def parser(grammar, s):
  ps = grammar.match(Grammar(_), lambda ps: ps).end
  tokens = tokenize(ps, s)
  #print(tokens)

  r = parse(ps, tokens)
  print('R:', r)
  if not r is None:
    (p, t) = r
    if len(t) == 0:
      return p
  print('Syntax error occurred, input could not be parsed.')
  return None


#######################################################
## Interaction

def interact():
  print('Interactive parser. Submit \':q\' or \':quit\' to exit.')
  # Interactive loop.
  while True:
    # Prompt the user for a query.
    s = input('> ')
    if s == 'q' or s == ':q':
      break
  
    # Parse the query.
    r = parser(grammar, s)
    if not r is None:
      pprint.pprint(r)
    else:
      print('Unknown input.')
    print()


#######################################################
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


#eof
