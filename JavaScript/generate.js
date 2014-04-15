/********************************************************************
** 
** generate.js
**
** A cross-platform parser library.
**
**   Web:     imparse.org
**   Version: 0.0.0.4
**
*/

(function (_, uxadt, richreports, imparse) {

  // Synonyms.
  var U = uxadt;
  var RR = richreports;
  var I = imparse;

  ///////////////////////////////////////////////////////////////////
  // BNF to UxADT grammar

  I.genGrammar = I.Grammar([
    I.Production('Comment', [
      I.Choices([
        I.Choice('Comment', AssocNone(), [ I.RegExpr('/' + I.genComment + '/') ]),
        ]),
      ]),

    I.Production('Grammar', [
      I.Choices([
        I.Choice('Grammar', AssocNone(), [ I.Many([I.Nonterminal('Production')]) ]),
        ]),
      ]),

    I.Production('Production', [
      I.Choices([
        I.Choice('Production', AssocNone(), [
          I.May([I.Nonterminal('Comment')]), I.RegExpr('/' + I.genProd + '/'),
            I.Terminal('::='), I.May([I.Nonterminal('Choice')])
          ]),
        ]),
      ]),

    I.Production('Choice', [
      I.Choices([
        I.Choice('Choice', AssocNone(), [
          I.RegExpr('/' + I.label + '/'), I.Terminal('|'), I.May([I.Nonterminal('Choice')])
          ]),
        ]),
      ]),

    I.Production('Expression', [
      I.Choices([
        I.Choice('May', AssocNone(), [
          I.RegExpr('['), I.Many([I.Nonterminal('Expression')]), I.Terminal(']')
          ]),
        I.Choice('MayMany', AssocNone(), [
          I.Nonterminal('Expression'), I.Terminal('*')
          ]),
        I.Choice('MayMany', AssocNone(), [
          I.RegExpr('{'), I.Many([I.Nonterminal('Expression')]), I.Terminal('}')
          ]),
        I.Choice('Many', AssocNone(), [
          I.Nonterminal('Expression'), I.Terminal('+')
          ]),
        ]),
      I.Choices([
        I.Choice('RegExpr', AssocNone(), [
          I.RegExpr('/' + regExpr + '/')
          ]),
        I.Choice('Nonterminal', AssocNone(), [
          I.RegExpr('/' + nonterminal + '/')
          ]),
        I.Choice('EmptyString', AssocNone(), [
          I.RegExpr('\"\"')
          ]),
        I.Choice('Terminal', AssocNone(), [
          I.RegExpr('/\"[' + reserved + ']\"/')
          ]),
        I.Choice('Terminal', AssocNone(), [
          I.RegExpr('/' + terminal + '/')
          ]),
        ]),
      I.Choices([
        I.Choice('Group', AssocNone(), [
          I.RegExpr('('), I.Many([I.Nonterminal('Expression')]), I.Terminal(')')
          ]),
        ]),
      ]) // EO Expression
    ]); // End of grammar definition

  ///////////////////////////////////////////////////////////////////
  // Generate UxADT from BNF (text).

  I.bnfToUxadt = function(bnf) { // OR INPUT???
    var tokens = I.tokenize(I.genGrammar)(bnf);
    /*(function() { // Tokenize BNF
      var re = [comment,
                   regExpr,
                  '[' + reserved + ']',
                  '\\\"[' + reserved + ']\\\"', // Reserved characters as terminals
                  '::=|[^\s' + reserved + ']+', // Terminals and nonterminals
                  ];
      var whitespace = new RegExp(/\s/); 
      var tokens = [];
      while (bnf.length > 0) {
        if (bnf[0].match(whitespace)) {
          bnf = bnf.slice(1);
        } else {
          for (var i = 0; i < re.length; i++) {
            var m = s.match('^' + re[i]);
            if (m != null) {
              tokens.push(m[0]);
              bnf = bnf.slice(m[0].length);
              break;
            }
          }
        }
      }
      return tokens;
      })(); */ // <-- This should be faster however.
    var r = I.parser(I.genGrammar)(tokens);
    if (r != null) {
      // preliminary parse tree BNF -> UxADT
      document.getElementById('bnfOutputTree').innerHTML = JSON.stringify(tree); // Pretty print 
      // Returns uxadt
      return function(r) {
        var ps = parse_tree.Grammar;
        var psUxadt = [];
        for (int i = 0; i < ps.length; i++) {
          var prodName = ps[i].production[0];
          var cs = ps[i].production.slice(1);
          var cb = [];
          for (int j = 0; j < cs.length; j++) {
            var c = cs[j].Choice;
            var name = function(l) { return l != 'None' ? l : null; }(c[0]);
            c = c.slice(1);
            var seq = [];
            while (c.length > 0) {
              var r = toUxadt(c[0]);
              if (r != null) {
                seq.push(r);
                c = c.slice(1);
              } else break;
            }
            cb.push(I.Choice(name, I.AssocNone(), seq));
          }
          psUxadt.push(I.Production(prodName, [I.Choices(cb)]));
        }
        return I.Grammar(psUxadt);
      }(r);
    } else {
      document.getElementById('bnfOutputTree').innerHTML = 'Could not convert BNF to UxADT.';
      return null;
    }
  } 

  I.toUxadt = function(es) {
    if (typeof(es) === 'string') return [I.Terminal(es)];

    var cs = [];
//    var ty = _.keys(es)[0];
//    var x = es.ty[0];

    es._(I.Comment(_), function(c) {
        var whitespace = new RegExp(/\s/);
        while (c[0] === '#' || c[0] = whitespace)
          c = c.slice(1);
        if (c.slice(-1)[0]) === '\n') x.pop();
        cs.push(I.Comment(c));
        })
      ._(I.Terminal(_), function(t) {
        if (t.length === 3 && t[0] === '\"' && t[2] === '\"') t = t[1];
        cs.push(I.Terminal(t));
        })
      ._(I.EmptyString(_), function(nt) {
        cs.push(I.Terminal('\"\"'));
        })
      ._(I.Nonterminal(_), function(nt) {
        cs.push(I.Nonterminal(t));
        })
      ._(I.RegExpr(_), function(re) {
        re = '/' + re.slice(1, -1) + '/';
        cs.push(I.RegExpr(re));
        })
      ._(I.One(_), function(x) { // May just change this to act like may (jsut here)
        x = I.toUxadt(x);
        cs.push(I.One(x));
        })
      ._(I.May(_), function(x) {
        var m = [];
        for (int i = 0; i < x.length; i++)
          m.push(I.toUxadt(x[i]));
        cs.push(I.May(m));
        })
      ._(I.Many(_), function(x) {
        var m = [];
        for (int i = 0; i < x.length; i++)
          m.push(I.toUxadt(x[i]));
        cs.push(I.Many(m));
        })
      ._(I.MayMany(_), function(x) {
        var m = [];
        for (int i = 0; i < x.length; i++)
          m.push(I.toUxadt(x[i]));
        cs.push(I.MayMany(m));
        })
      ._(I.Group(_), function(x) {
        for (int i = 0; i < x.length; i++)
          cs.push(I.toUxadt(x));
        })
      .end;
    return cs;
  }

  ///////////////////////////////////////////////////////////////////
  // UxADT to text

  I.writeUxadt = function (g, indent = 2) {
    document.getElementById('bnfOutputUxadt').innerHTML = function() {
      if (indent > 4) indent = 4;
      var ws = '                ';
      var output = 'I.Grammar(['
      var ps = g._(I.Grammar(_), function(ps) { return ps; }).end; // Productions
      while (ps.length > 0) {
        var p = uxadt(ps[0]).map(I.Production(_, _), function(nt, cbs) { return [nt, cbs]; })); // Nonterminal, Choice blocks
        ps = ps.slice(1);
        output += '\n' + ws.substr(0, indent) + 'I.Production(\'' + p[0] + '\', [';
        while (p[1].length > 0) {
          var cb = uxadt(p[1]).map(I.Choices(_), function(cb) { return cb; })); // Choices
          p[1] = p[1].slice(1);
          output += '\n' + ws.substr(0, indent * 2) + 'I.Choices([';
          for (int i = 0; i < cb.length; i++) {
            var c = uxadt(cb[i]).map(I.Choice(_, _, _), function(l, a, seq) { return [l, seq]; })); // Choice
            if (c[0] == null) c[0] = 'None';
            output += '\n' + ws.substr(0, indent * 3) + 'I.Choice(\'' + c[0] + '\', I.AssocNone(), [';
            var seq = uxadtExprStr(seq);
            output += '\n' + ws.substr(0, indent * 4) + seq + '\n' + indent.repeat(4) + ']),'; // End of Choice
          }
          output += '\n' + wb.substr(0, indent * 3)  + ']),'; // End of Choice Block
        }
        output += '\n' + wb.substr(0, indent * 2) + ']),'; // End of Production
      }
      return output + '\n' + ws.substr(0, indent) + '])'; // End of Grammar
    };
  }

  I.uxadtExprStr = function(seq) {
    var s = '';

    for (int i = 0; i < seq.length; i++) {
      seq[i] // Append appriopriately to s
        ._(I.Terminal(_), function(t) { s += 'Terminal(\'' + t + '\'), '; })
        ._(I.Nonterminal(_), function(nt) { s += 'Nonterminal(\'' + t + '\'), '; })
        ._(I.RegExpr(_), function(re) { s += 'RegExpr(\'' + t + '\'), '; })
        ._(I.One(_), function(x) { s += 'One([' + uxadtExprStr(x) + ']), '; })
        ._(I.Many(_), function(x) { s += 'Many([' + uxadtExprStr(x) + ']), '; })
        ._(I.MayMany(_), function(x) { s += 'MayMany([' + uxadtExprStr(x) + ']), '; })
        .end;
    }
    return s;
  }
  
/*
  I.toUxadt = function(parse_tree) {
    var ps = parse_tree.Grammar;
    var psUxadt = [];
    for (int i = 0; i < ps.length; i++) {
      var prodName = ps[i].production[0];
      var cs = ps[i].production.slice(1);
      var cb = [];
      for (int j = 0; j < cs.length; j++) {
        var c = cs[j].Choice;
        var name = function(l) { return l != 'None' ? l : null; }(c[0]);
        c = c.slice(1);
        var seq = [];
        while (c.length > 0) {
          var r = toUxadtExpr(c[0]);
          if (r != null) {
            seq.push(r);
            c = c.slice(1);
          } else break;
        }
        cb.push(I.Choice(name, I.AssocNone(), seq));
      }
      psUxadt.push(I.Production(prodName, [I.Choices(cb)]));
    }
    return I.Grammar(psUxadt);
  } */

/* eof */
