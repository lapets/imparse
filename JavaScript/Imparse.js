/********************************************************************
** 
** imparse.js
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
  // Data type definitions.

  U.define(imparse, {
      Just: [_],
      Nothing: []
    });

  U.define(imparse, { Grammar: [_] });
  U.define(imparse, { Production: [_, _] });

  U.define(imparse, {
      AssocNone: [],
      AssocLeft: [],
      AssocRight: [],
      AssocFlat: []
    });

  U.define(imparse, { Choices: [_] });
  U.define(imparse, { Choice: [_, _, _] });
    
  U.define(imparse, {
      Nonterminal: [_],
      RegExpr: [_],
      Terminal: [_]
    });

  U.define(imparse, {
      Normal: [],
      IndentPresentation: [],
      IndentRequired: []
    });
 
  U.define(imparse, {
      One: [],
      May: [],
      Many: [_],
      MayMany: [_]
    });

  ///////////////////////////////////////////////////////////////////
  // Functionality.   

  I.tokenizer = function(grammar) {
    var ps = grammar._(I.Grammar(_), function(ps) { return ps; }).end;
    var css = _.flatten(uxadt(ps).map(I.Production(_, _), function(nt, css) { return css; }));
    var cs = _.flatten(uxadt(css).map(I.Choices(_), function(cs) { return cs; }));
    var es = _.flatten(uxadt(cs).map(I.Choice(_, _, _), function(l, a, seq) { return seq; }));
    var ts = uxadt(es).map(I.Terminal(_), function(t) { return t; });
    var rs = uxadt(es).map(I.RegExpr(_), function(r) { return r; });

    return function(s) {
      var tokens = [];
      var whitespace = new RegExp(/\s/);
      while (s.length > 0) {
        if (s[0].match(whitespace))
          s = s.slice(1);
        else {
          for (var i = 0; i < ts.length; i++) {
            if (s.indexOf(ts[i]) === 0) {
              tokens.push(s.slice(0,ts[i].length));
              s = s.slice(ts[i].length);
              break;
            }
          }
          for (var i = 0; i < rs.length; i++) {
            var m = s.match("^"+rs[i]);
            if (m != null) {
              tokens.push(m[0]);
              s = s.slice(m[0].length);
              break;
            }
          }
        }
      }
      return tokens;
    }
  }

  I.parse = function(g, tmp, nt, cix) {
    return g
      ._(I.Grammar(_), function(ps) {
        if (typeof cix == 'undefined') cix = 0; // Default choice block.
        if (typeof nt  == 'undefined')          // Default non-terminal.
          nt = uxadt(ps).find(I.Production(_, _), function(nt, cs) { return nt; });

        // Parse the token sequence using the production corresponding
        // to the specified nonterminal.
        return uxadt(ps).find(I.Production(_, _), function(ntP, choiceBlocks) {
          if (ntP === nt) {
            var cr = null;            
            for ( ; cix < choiceBlocks.length && cr == null; cix++) { // Select appropriate choice block.
              cr = choiceBlocks[cix]._(I.Choices(_), function(choices) {
                // Try each sequence.
                return uxadt(choices).find(I.Choice(_, _, _), function(name, assoc, sequence) {
                  var tokens = tmp.slice();
                  var terminal_counter = 0;
                  var subs = [];
                  for (var r = true, k = 0; k < sequence.length && tokens.length > 0 && r != false; k++) {
                    var r = sequence[k]
                      ._(I.Nonterminal(_), function(ntSeq) {
                        var child_tokens = I.parse(g, tokens, ntSeq, (k == 0 && ntSeq == nt) ? cix+1 : 0);
                        if (child_tokens != null) {
                          subs.push(child_tokens[0]);
                          tokens = child_tokens[1];
                        } else return false;
                      })
                      ._(I.RegExpr(_), function(r) {
                        if (tokens[0].match(new RegExp(r))) {
                          subs.push(tokens[0]);
                          tokens = tokens.slice(1);
                        } else return false;
                      })
                      ._(I.Terminal(_), function(t) {
                        if (tokens[0] == t) {
                          tokens = tokens.slice(1);
                          terminal_counter++;
                        } else return false;
                      })
                      .end;
                  }
                  if (terminal_counter + subs.length == sequence.length)
                    return [name == null ? subs[0] : uxadt.constructor(name).apply(this, subs), tokens.slice()];
                }); // Try the choices.
              }).end; // Select choice block.
            } // Iterate through choiceBlocks.
            return cr;
          } // Only use production with matching target non-terminal.
        }); // Pick out the right production.
      }).end; // Unwrap grammar.
  } // parse()

  I.parser = function(grammar) {
    return function (tokens, nonterminal) {
      return I.parse(grammar, tokens, nonterminal);
    };
  }

  I.tokenizerParser = function(grammar) {
    var tokenizer = I.tokenizer(grammar);
    return function (s) {
      return I.parse(grammar, tokenizer(s));
    };
  }

})(_, uxadt, richreports, this.imparse =

  // Build a tokenizer/parser using the top-level
  // module name as a constructor.

  function(grammar){
    var tokenizer = this.imparse.tokenizer(grammar);
    return function (s) {
      return this.imparse.parse(grammar, tokenizer(s));
    };
  }
);

/* eof */