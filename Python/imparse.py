#######################################################
##
## Imparse.py
##
## A cross-platform parser library.
##
##    Web:     imparse.org
##    Version: 0.0.0.4
##
#######################################################

import re
import uxadt as U

exec(open("uxadt.py").read())

#######################################################
## Data type definitions

eval(U.uxadt.definition({\
  'Just': [_],\
  'Nothing': []\
  }))

eval(U.uxadt.definition({ 'Grammar': [_]}))
eval(U.uxadt.definition({ 'Production': [_, _] }))

eval(U.uxadt.definition({\
  'AssocNone': [],\
  'AssocLeft': [],\
  'AssocRight': [],\
  'AssocFlat': []\
  }))

eval(U.uxadt.definition({ 'Choices': [_] }))
eval(U.uxadt.definition({ 'Choice': [_, _, _] }))

eval(U.uxadt.definition({\
  'Nonterminal': [_],\
  'RegExpr': [_],\
  'Terminal': [_]\
  }))

eval(U.uxadt.definition({\
  'Normal': [],\
  'IndentPresentation': [],\
  'IndentRequired': []\
  }))

eval(U.uxadt.definition({\
  'One': [],\
  'May': [],\
  'Many': [_],\
  'MayMany': [_]\
  }))


#######################################################
## Functionality

def parser(grammar, s):
  ps = grammar.match(Grammar(_), lambda ps: ps).end
  topNonterminal = ps[0].match(Production(_, _), lambda nt, cbs: nt).end

  # Retrieve terminals
  terminals = []
  cbs = [cb for p in ps for cb in p.match(Production(_, _), lambda nt,cbs: cbs).end]
  cs = [c for cb in cbs for c in cb.match(Choices(_), lambda cs: cs).end]
  for c in cs:
    seq = c.match(Choice(_, _, _), lambda l,a,seq: seq).end
    for x in seq:
      if etype(x) == "t":
        t = x.match(Terminal(_), lambda t: t).end
        if t in "()+*": t = "\\" + t
        terminals = terminals + [t]

  #def parse(tmp, nt = topNonterminal):
  def parse(tmp, nt = None):
    # Tokenize tmp if it is a string
    if type(tmp) == str:
      tmp = [t for t in re.split("(\s+|"+"|".join(terminals)+")", tmp)]
      tmp = [t for t in tmp if not (t == None or t.isspace() or t == "")]

    for p in ps:
      pnt = p.match(Production(_, _), lambda nt,cbs: nt).end

      # If a nonterminal has been provided as input, skip over productions until
      # the nonterminal and production's nonterminal match.
      if nt is not None and pnt != nt:
        pass

      # Proceed as normal
      else:
        cbs = p.match(Production(_, _), lambda nt,cbs: cbs).end
        for cb in cbs:
          cs = cb.match(Choices(_), lambda cs: cs).end

          for c in cs:
            (label, a, seq) = c.match(Choice(_, _, _), lambda l,a,seq: (l,a,seq)).end
            (ts, es, tokens) = (0, [], tmp[0:])
            if len(tmp) == 0:
              if len(seq) == 0:
                return (label, [])

            for x in seq:
              # Regular expression
              if etype(x) == "r":
                r = x.match(RegExpr(_), lambda r: r).end
                if r[0] == "/" and r[-1] == "/":
                  if len(tokens) > 0 and re.compile(r[1:-1]).match(tokens[0]):
                    es = es + [tokens[0]]
                    tokens = tokens[1:]
                  else: break

              # Terminal
              elif etype(x) == "t":
                t = x.match(Terminal(_), lambda t: t).end
                if len(tokens) > 0 and tokens[0] == t:
                  tokens = tokens[1:]
                  ts = ts + 1
                else: break

              # Nonterminal
              else:
                nt2 = x.match(Nonterminal(_), lambda nt: nt).end
                r = parse(tokens, nt2)
                if not r is None:
                  (e, tokens) = r
                  es = es + [e]
            if ts + len(es) == len(seq):
              return ({label:es} if len(es) > 0 else label, tokens)

  r = parse(s)
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
    if s == ':quit' or s == ':q':
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
    .match(Terminal(_), lambda t: "t")\
    .match(RegExpr(_), lambda r: "r")\
    .match(Nonterminal(_), lambda nt: "nt")\
    .end
