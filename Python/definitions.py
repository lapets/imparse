import uxadt as U
exec(open('uxadt.py').read())

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
  'One': [_],\
  'May': [_],\
  'Many': [_],\
  'MayMany': [_]\
  }))
