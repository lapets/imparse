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
      if etype(x) == "t":
        t = re.escape(x.match(Terminal(_), lambda t: t).end)
        if t not in terminals:
          terminals = terminals + [t]

  tmp = [t for t in re.split(r"(\s+|"+"|".join(terminals)+")", s)]
  tokens = [t for t in tmp if not (t == None or t.isspace() or t == "")]
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

        incseq = 0
        for x in seq:
          et = etype(x)
          if et is not None:
            (ty, expr) = et

          if et is None:
            (ty, seq2) = ptype(x)
            r = parExpr(ps, tokens, (ty, seq2))
            if r is not None:
              if r == True:
                ts = ts + 1
              else:
                (e, tokens) = r
                es = es + [e]

              if ty == 'Many' or ty == 'MayMany':
                while r is not None and r != True:
                  r = parExpr(ps, tokens, (ty, seq2))
                  if r is not None:
                    if r is True:
                      ts = ts + 1
                      incseq = incseq + 1
                    else:
                      (e, tokens) = r
                      es = es + [e]
                      incseq = incseq + 1
            else: break

          # Terminal
          elif ty == 't':
            if len(tokens) > 0 and tokens[0] == expr:
              tokens = tokens[1:]
              ts = ts + 1
            else: break

          # Regular expression
          elif ty == 'r':
            if expr[0] == '/' and expr[-1] == '/':
              if len(tokens) > 0 and re.compile(expr[1:-1]).match(tokens[0]):
                es = es + [tokens[0]]
                tokens = tokens[1:]
              else: break
          
          # Nonterminal
          elif ty == 'nt':
            if ts + len(es) == 0:
              if expr == nt and leftFactor == True: # Top nonterminal
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

        if ts + len(es) == len(seq) + incseq:
          if label is None and len(es) == 1:
            return (e, tokens)
          else:
            return ({label:es} if len(es) > 0 else label, tokens)
  


def parExpr(ps, tokens, e):
  (ty, seq) = e
  if ty == 'May' or ty == 'MayMany':
    may = True
  else:
    may = False
  ts = 0
  es = []

  for x in seq:
    (ety, expr) = etype(x)

    if ety is None:
      (ty, seq2) = ptype(x)
      r = parExpr(ps, tokens, (ty, seq2))
      if r is not None:
        if r == True:
          ts = ts + 1
        else:
          (e, tokens) = r
          es = es + [e]

        if ty == 'Many' or ty == 'MayMany':
          while r is not None and r != True:
            r = parExpr(ps, tokens, (ty, seq2))
            if r is not None:
              if r is True:
                ts = ts + 1
                incseq = incseq + 1
              else:
                (e, tokens) = r
                es = es + [e]
                incseq = incseq + 1
      else: break

    # Terminal
    elif ety == 't':
      t = x.match(Terminal(_), lambda t: t).end
      if len(tokens) > 0 and tokens[0] == t:
        tokens = tokens[1:]
        ts = ts + 1
      else: break

    # Regular expression
    elif ety == 'r':
      regex = x.match(RegExpr(_), lambda r: r).end
      if regex[0] == '/' and regex[-1] == '/':
        if len(tokens) > 0 and re.compile(regex[1:-1]).match(tokens[0]):
          es = es + [tokens[0]]
          tokens = tokens[1:]
        else: break

    # Nonterminal
    elif ety == 'nt':
      nt = x.match(Nonterminal(_), lambda nt: nt).end
      r = parse(ps, tokens, nt2, False)
      if r is not None:
        (e, tokens) = r
        es = es + [e]
      else: break

  if ts + len(es) == len(seq):
    if len(es) == 1:
      return (es[0], tokens)
    else:
      return (es, tokens)
  else:
    if may:
      return True
    else:
      return None


def parser(grammar, s):
  ps = grammar.match(Grammar(_), lambda ps: ps).end
  tokens = tokenize(ps, s)

  r = parse(ps, tokens)
#  print('R:', r)
  if not r is None:
    (p, t) = r
    if len(t) == 0:
      return p
  print("Syntax error occurred, input could not be parsed.")
  return None


#######################################################
## Interaction

def interact():
  print("Interactive parser. Submit \":q\" or \":quit\" to exit.")
  # Interactive loop.
  while True:
    # Prompt the user for a query.
    s = input('> ')
    if s == 'q' or s == ':q':
      break
  
    # Parse the query.
    r = parser(grammar, s)
    if not r is None:
      print(r)
    else:
      print("Unknown input.")
    print()


#######################################################
## Useful helper function.

def etype(e):
  return e\
    .match(Terminal(_), lambda t: ("t", t))\
    .match(RegExpr(_), lambda r: ("r", r))\
    .match(Nonterminal(_), lambda nt: ("nt", nt))\
    .end

def ptype(e):
  return e\
    .match(One(_), lambda seq: ("One", seq))\
    .match(May(_), lambda seq: ("May", seq))\
    .match(Many(_), lambda seq: ("Many", seq))\
    .match(MayMany(_), lambda seq: ("MayMany", seq))\
    .end


#eof
