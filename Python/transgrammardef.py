g = Grammar([\
	Production('Grammar', [\
		Choices([
			Choice('Grammar', AssocNone(), [\
				Many([Nonterminal('Production'), ]), \
				]),\
			]),\
		]),\
	Production('Production', [\
		Choices([
			Choice('Production', AssocNone(), [\
				RegExpr('/[a-z][A-Za-z]*/'), Terminal('::='), Many([Nonterminal('Choice'), ]), \
				]),\
			]),\
		]),\
	Production('Choice', [\
		Choices([
			Choice('Choice', AssocNone(), [\
				RegExpr('/[A-Z][A-Za-z]*/'), Terminal('|'), Many([Nonterminal('Expression'), ]), \
				]),\
			]),\
		]),\
	Production('Expression', [\
		Choices([
			Choice('May', AssocNone(), [\
				Terminal('['), Many([Nonterminal('Expression'), ]), Terminal(']'), \
				]),\
			Choice('MayMany', AssocNone(), [\
				Nonterminal('Expression'), Terminal('*'), \
				]),\
			Choice('MayMany', AssocNone(), [\
				Terminal('{'), Many([Nonterminal('Expression'), ]), Terminal('}'), \
				]),\
			Choice('Many', AssocNone(), [\
				Nonterminal('Expression'), Terminal('+'), \
				]),\
			]),\
		Choices([
			Choice('Terminal', AssocNone(), [\
				RegExpr('/([^\s`][^\s]+)/'), \
				]),\
			Choice('Nonterminal', AssocNone(), [\
				RegExpr('/<([A-Z][A-Za-z0-9]*)>/'), \
				]),\
			Choice('RegExpr', AssocNone(), [\
				RegExpr('/\$([^\s]*)\$/'), \
				]),\
			Choice('Empty String', AssocNone(), [\
				Terminal('""'), \
				]),\
			]),\
		Choices([
			Choice('Group', AssocNone(), [\
				Terminal('('), Many([Nonterminal('Expression'), ]), Terminal(')'), \
				]),\
			]),\
		]),\
	])