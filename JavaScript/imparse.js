/* ********************************************************
** 
** imparse.js
**
** A cross-platform parser library.
**
**   Web:     imparse.org
**   Version: 0.0.0.4
**
*/

(function (_, uxadt) {
  var self = this;

  var imparse = function (obj) {
    if (obj instanceof imparse)        return obj;
    if (!(this instanceof imparse))    return new imparse(obj);
    this._wrapped = obj;
  };

  // Useful synonyms
  var U = uxadt;
  var I = imparse;

  ///////////////////////////////////////////////////////////////////
  // Data type definitions.

  U.define(imparse, {
    Just: [_],
    Nothing: [],

    Grammar: [_],
    Production: [_, _],
    Choices: [_],
    Choice: [_, _, _],
    Nonterminal: [_],
    RegExpr: [_],
    Terminal: [_],
    One: [],
    May: [],
    Many: [_],
    MayMany: [_],

    AssocNone: [],
    AssocLeft: [],
    AssocRight: [],
    AssocFlat: [],

    Normal: [],
    IndentPresentation: [],
    IndentRequired: []
  });

  ///////////////////////////////////////////////////////////////////
  // Parsing functionality.

  // Tokenizes string according to grammar to parse.
  I.tokenize = function (grammar, str) {
    var ps = grammar._(I.Grammar(_), function (ps) { return ps; }).end; // Productions
    var cbs = _.flatten(uxadt(ps).map(I.Production(_, _), function (nt, cbs) { return cbs; })); // Choice blocks
    var cs = _.flatten(uxadt(cbs).map(I.Choices(_), function (cs) { return cs; })); // Choices
    var es = _.flatten(uxadt(cs).map(I.Choice(_, _, _), function (l, a, seq) { return seq; })); // Choice sequences

    var getTermRegex = function (seq) {
      var terminals = [];
      var regex = [];
      for (var i = 0; i < seq.length; i++) {
        seq[i]
        ._(I.Terminal (_), function (t) {
          terminals.push(t);
        })
        ._(I.RegExpr(_), function (re) {
          regex.push(re);
        })
        ._(I.One(_), function (inseq) {
          var r = getTermRegex(inseq);
          terminals.extend(r[0]);
          regex.extend(r[1]);
        })
        ._(I.May(_), function (inseq) {
          var r = getTermRegex(inseq);
          terminals.extend(r[0]);
          regex.extend(r[1]);
        })
        ._(I.Many(_), function (inseq) {
          var r = getTermRegex(inseq);
          terminals.extend(r[0]);
          regex.extend(r[1]);
        })
        ._(I.MayMany(_), function (inseq) {
          var r = getTermRegex(inseq);
          terminals.extend(r[0]);
          regex.extend(r[1]);
        })
        .end;
      }
      return [terminals, regex];
    };

    
    var tmp = getTermRegex(es);
    var ts = tmp[0];
    var rs = tmp[1];

    return (function (s) {
      var tokens = [];
      var whitespace = new RegExp(/\s/);
      while (s.length > 0) {
        if (s[0].match(whitespace)) {
          s = s.slice(1);
        } else {
          var len = s.length;
          // Match with terminal.
          for (var i = 0; i < ts.length; i++) {
            if (s.indexOf(ts[i]) === 0) {
              tokens.push(s.slice(0,ts[i].length));
              s = s.slice(ts[i].length);
              break;
            }
          }
          // Match with regular expression.
          for (i = 0; i < rs.length; i++) {
            var m = s.match("^" + rs[i]);
            if (m != null) {
              tokens.push(m[0]);
              s = s.slice(m[0].length);
              break;
            }
          }
          if (len == s.length) break;
        }
      }
      if (s.length > 0) { // Unable to tokenize string.
        return [false, s];
      }
      return tokens;
    })(str);
  }; // tokenize()

  // Parsing function. Goes through grammar productions and choices and attempt
  // to parse tokens.
  I.parse = function (grammar, tmp, nt, lf) {
    if (nt == 'undefined') nt = null;     // Default top non-terminal.
    if (lf == 'undefined') lf = false;    // Left factoring flag off by default.

    var ps = grammar._(I.Grammar(_), function (ps) { return ps; }).end; // Productions

    return uxadt(ps).find(I.Production(_, _), function (pnt, choiceBlocks) { // Select appropriate production.
      if (pnt != nt && nt != null) {
        // Skip production.
      } else {
        return uxadt(choiceBlocks).find(I.Choices(_), function (choices) { // Select appropriate choice block.
          return uxadt(choices).find(I.Choice(_, _, _), function (label, a, seq) { // Select appropriate choice.
            var tokens = tmp.slice();
            if (tokens.length == 0 && seq.length == 0) { return [label, []]; }
            var result;
            if (nt == null) { // No top non-terminal defined, try with current production instead.
              result = I.parseSeq(grammar, tokens, seq, label, [pnt, pnt], lf);
            } else { 
              result = I.parseSeq(grammar, tokens, seq, label, [nt, pnt], lf);
            }
            if (result != null && result != true) {
              return result;
            }
          }); // Try each sequence/choice.
        }); // Try choice blocks.
      }
    }); // Try productions.
  }; // parse()

  // Parse sequences of expressions (eg. for a choice or nested sequence instead of May, Many, etc).
  I.parseSeq = function (grammar, tokens, seq, label, nt, lf) {
    var terminalCounter   = 0;      // Counter for terminals in the sequence.
    var inseqCounter      = 0;      // Counter for nested sequences (eg. in May, Many, etc).
    var es                = [];     // Parsed expressions.
    var pnt               = null;   // Default production non-terminal.
    if (label == 'undefined') label = null;   // Default label.
    if (lf == 'undefined') lf = false;        // Left factoring off by default.
    if (nt == 'undefined') nt = null;         // Default top non-terminal.
    if (nt != null) {
      pnt = nt[1];
      nt = nt[0];
    }

    // Iterate through expressions in the sequence.
    for (var i = 0, x; i < seq.length && x != false; i++) {
      x = seq[i]
      ._(I.Terminal(_), function (t) {
        if (tokens[0] == t) {
          tokens = tokens.slice(1);
          terminalCounter++;
        } else return false;
      })
      ._(I.RegExpr(_), function (r) {
        if (typeof r == "string") r = new RegExp(r);
        if (tokens[0].match(r)) {
          es.push(tokens[0]);
          tokens = tokens.slice(1);
        } else return false;
      })
      ._(I.Nonterminal(_), function (nt2) {
        var r;
        if (terminalCounter + es.length == 0) {
          if (nt2 == nt && lf) { // Left-factoring flag is on but non-terminals match; skip this sequence.
            return false;
          } else if (nt2 == nt) { // Non-terminals match; attempt to left-factor.
            r = I.parse(grammar, tokens, nt2, true);
          } else { // Distinct non-terminal was encountered at the start of the sequence.
            r = I.parse(grammar, tokens, nt2, false);
          }
        } else {
          // Non-terminal is not at the beginning of the sequence.
          r = I.parse(grammar, tokens, nt2, false);
        }
        if (r != null) {
          es.push(r[0]);
          if (_.isArray(r[0])) {
            terminalCounter += r[0].length;
          }
          tokens = r[1];
          lf = false;
        } else return false;
      })
      ._(I.One(_), function (innerSeq) {
        var r = I.parseSeq(grammar, tokens, innerSeq);
        if (r != null) {
          if (r[0].length == 0) {
            terminalCounter++;
          } else es.push(r[0]); 
          tokens = r[1]; 
        } else return false;
      })
      ._(I.May(_), function (innerSeq) { // May
        var r = I.parseSeq(grammar, tokens, innerSeq);
        if (r != null) {
          if (r[0].length == 0) {
            terminalCounter++;
          } else es.push(r[0]); 
          tokens = r[1]; 
        } else { // No match was found. Continue.
          terminalCounter++; // Consume expression, increment towards final counter.
        }
      })
      ._(I.Many(_), function (innerSeq) {
        // Greedily matches tokens with the sequence.
        var c = 0, r;
        do {
          r = I.parseSeq(grammar, tokens, innerSeq);
          if (r != null) {
            if (r[0] == []) {
              terminalCounter++;
              if (c > 0) inseqCounter++;
            } else {
              es.push(r[0]);
              if (c > 0) inseqCounter++;
            }
            tokens = r[1];
            c++;
          } else if (c == 0) {
            return false; // No match was found.
          }
        } while (r != null && innerSeq.length > 0 && tokens.length > 0);
      })
      ._(I.MayMany(_), function (innerSeq) {
        // Greedily matches tokens with the sequence.
        var c = 0, r;
        do {
          r = I.parseSeq(grammar, tokens, innerSeq);
          if (r != null) {
              if (r[0].length == 0) {
              terminalCounter++;
              if (c > 0) inseqCounter++;
            } else {
              es.push(r[0]);
              if (c > 0) inseqCounter++;
            }
            tokens = r[1]; 
            c++;
          } else if (c == 0) { // No match was found. Continue.
            terminalCounter++; // Consume expression, increment towards final counter.
          }
        } while (r != null && innerSeq.length > 0 && tokens.length > 0);
      })
      .end;
    }

    if (terminalCounter + es.length === seq.length + inseqCounter) {
      if (label == null) {
        if (es.length == 1) {
          return [es[0], tokens.slice()]; // Single expression w/o label.
        }
        return [es, tokens.slice()]; // Many expressions w/o label.
      } else {
        if (es.length > 0) return [U.constructor(label).apply(this, es), tokens]; // Label with parsed expressions.
        return [label, tokens.slice()]; // Just the label.
      }
    } else {
      return null; // Unable to parse.
    }
  }; // parseSeq()

  // Concise function which tokenizes and parses a string according to
  // the input grammar.
  I.parser = function (grammar, str) {
    var tokens = I.tokenize(grammar, str);
    var pt = I.parse(grammar, tokens);
    if (pt != null) {
      if (pt[1].length == 0) { // Parsing successful (no remaining tokens). 
        return pt[0]; // Return parse tree
      } else {
        return "Syntax error, input could not be parsed.";
      }
    }
    return "Syntax error, input could not be parsed."; 
  }; // parser()


  ///////////////////////////////////////////////////////////////////
  // Pretty print UxADT grammar for Javascript.
  I.printUxadtGrammar = function (grammar, indent, synonym) {
    if (typeof synonym == "undefined")  synonym = "I";    // Default synonym for imparse = I.
    if (typeof indent == "undefined")   indent = 2;       // Default indentation = 2.
    if (indent > 8)                     indent = 6;       // Max indentation = 8.
    indent = Array(indent + 1).join(" ");
    var output = synonym + ".Grammar([";
    var ps = grammar._(I.Grammar(_), function (ps) { return ps; }).end;

    // Helper function to get strings for sequences.
    var printUxadtSeq = function(seq, depth) {
      if (typeof depth == "undefined") depth = 5;
      var space = "\n" + Array(depth).join(indent);
      var str = "";
      var es = [];
      for (var i = 0; i < seq.length; i++) {
        if (_.isArray(seq[i])) {
          str += printUxadtSeq(seq[i], depth);
        } else {
          var e = seq[i]
          ._(I.Terminal(_), function (t) {
            return synonym + ".Terminal(\'" + t + "\')";
          })
          ._(I.RegExpr(_), function (r) {
            if (typeof r != "string") r = r.source;
            return synonym + ".RegExpr(\'" + r + "\')";
          })
          ._(I.Nonterminal(_), function (nt) {
            return synonym + ".Nonterminal(\'" + nt + "\')";
          })
          ._(I.One(_), function (inseq) {
            if (inseq.length < 3)   return synonym + ".One([ " + printUxadtSeq(inseq) + " ])"; 
            return synonym + ".One([ " + space + indent + printUxadtSeq(inseq, depth+1) + " ])"; 
          })
          ._(I.May(_), function (inseq) {
            if (inseq.length < 3)   return synonym + ".May([ " + printUxadtSeq(inseq) + " ])";
            return synonym + ".May([ " + space + indent + printUxadtSeq(inseq, depth+1) + " ])";
          })
          ._(I.Many(_), function (inseq) {
            if (inseq.length < 3)   return synonym + ".Many([ " + printUxadtSeq(inseq) + " ])";
            return synonym + ".Many([ " + space + indent + printUxadtSeq(inseq, depth+1) + "])";
          })
          ._(I.MayMany(_), function (inseq) {
            if (inseq.length < 3)   return synonym + ".MayMany([ " + printUxadtSeq(inseq) + " ])";
            return synonym + ".MayMany([ " + space + indent + printUxadtSeq(inseq, depth+1) + " ])";
          })
          .end;
          es.push(e);
          if (i < seq.length - 1)  es[i] += ", ";
        }
      }
      
      // Pretty spacing for expressions.
      var tmpstr = "";
      while (es.length > 0) {
        if (es.length > 1) {
          if ((tmpstr + es[0]).length <= 75) {
            tmpstr += es[0];
            es = es.slice(1);
          } else {
            str += tmpstr + space;
            tmpstr = es[0];
            es = es.slice(1);
          }
        } else {
          if ((tmpstr + es[0]).length <= 75) {
            str += tmpstr + es[0];
            es = es.slice(1);
          } else {
            str += tmpstr + space + es[0];
            es = es.slice(1);
          }
        }
      }
      return str;
    }; // End 

    for (var i = 0; i < ps.length; i++) {
      var p = ps[i]._(I.Production(_, _), function (nt, cbs) { return [nt, cbs]; }).end;
      output += "\n" + indent + synonym + ".Production(\'" + p[0] + "\', [";
      for (var j = 0; j < p[1].length; j++) {
        var cs = p[1][j]._(I.Choices(_), function (cs) { return cs; }).end;
        output += "\n" + Array(3).join(indent) + synonym + ".Choices([";
        for (var k = 0; k < cs.length; k++) {
          var c = cs[k]._(I.Choice(_, _, _), function (label, assoc, seq) { return [label, seq]; }).end;
          if (c[0] == null) c[0] = "None";
          output += "\n" + Array(4).join(indent) + synonym + ".Choice(\'" + c[0] + "\', " + synonym + ".AssocNone(), [";
          var seq = printUxadtSeq(c[1]);
          output += "\n" + Array(5).join(indent) + seq;
          output += "\n" + Array(4).join(indent) + "]),";
        } // Choices
        output += "\n" + Array(3).join(indent) + "]),"
      } // Choice blocks
      output += "\n" + indent + "]),"
    } // Productions
    output += "\n]);";
    return output;
  };

  ///////////////////////////////////////////////////////////////////
  // BNF -> UxADT
  // Regular expressions needed to tokenize and/or parse BNF to UxADT.
  self.reserved          = new RegExp(/(#\$\?\*\+\(\)\{\}\[\]\|)/); // Reserved characters
  self.comment           = new RegExp(/(#[^#]+#)/);
  self.regExpr           = new RegExp(/(\$[^$]+\$)/);
  self.production        = new RegExp(/([A-Z][A-Za-z0-9_]*)/);
  self.clabel            = new RegExp(/([A-Z][A-Za-z0-9_]*)/);
  self.nonterminal       = new RegExp(/(`[A-Z][A-Za-z0-9_]*)/);
  self.terminalRes       = new RegExp(/("[^A-Z\$\?\*\+\(\)\{\}\[\]\|]")/);
  self.terminal          = new RegExp(/([^A-Z\s#\$\?\*\+\(\)\{\}\[\]\|][^\s#\$\?\*\+\(\)\{\}\[\]\|]*)/);

  // Grammar definition for converting BNF to UxADT.
  I.genGrammar = I.Grammar([
    I.Production('Comment', [
      I.Choices([
        I.Choice('Comment', I.AssocNone(), [ I.RegExpr(self.comment) ])
      ])
    ]), 

    I.Production('Grammar', [
      I.Choices([
        I.Choice('Grammar', I.AssocNone(), [ I.Many([ I.MayMany([I.Terminal("\n")]), I.Nonterminal('Production') ]) ])
      ])
    ]),

    I.Production('Production', [
      I.Choices([
        I.Choice('Production', I.AssocNone(), [
          I.RegExpr(self.production), I.Terminal('::='), I.Many([ I.MayMany([I.Terminal("\n")]), I.Nonterminal('Choice') ])
        ])
      ])
    ]),

    I.Production('Choice', [
      I.Choices([
        I.Choice('Choice', I.AssocNone(), [
          I.RegExpr(self.clabel), I.Terminal('|'), I.Many([ I.Nonterminal('Expression') ])
        ])
      ])
    ]),

    I.Production('Expression', [
      I.Choices([
        I.Choice('May', I.AssocNone(), [ I.Terminal('['), I.Many([ I.Nonterminal('Expression') ]), I.Terminal(']') ]),
        I.Choice('May', I.AssocNone(), [ I.Nonterminal('Expression'), I.Terminal('?') ]),
        I.Choice('MayMany', I.AssocNone(), [ I.Nonterminal('Expression'), I.Terminal('*') ]),
        I.Choice('MayMany', I.AssocNone(), [ I.Terminal('{'), I.Many([ I.Nonterminal('Expression') ]), I.Terminal('}') ]),
        I.Choice('Many', I.AssocNone(), [ I.Nonterminal('Expression'), I.Terminal('+') ])
      ]),
      I.Choices([
        I.Choice('RegExpr', I.AssocNone(), [ I.RegExpr(self.regExpr) ]),
        I.Choice('Nonterminal', I.AssocNone(), [ I.RegExpr(self.nonterminal) ]),
        I.Choice('EmptyString', I.AssocNone(), [ I.Terminal('\"\"') ]),
        I.Choice('Terminal', I.AssocNone(), [ I.RegExpr(self.terminalRes) ]),
        I.Choice('Terminal', I.AssocNone(), [ I.RegExpr(self.terminal) ])
      ]),
      I.Choices([
        I.Choice('Group', I.AssocNone(), [ I.Terminal('('), I.Many([ I.Nonterminal('Expression') ]), I.Terminal(')') ])
      ])
    ])
  ]); // End of genGrammar

  // Tokenize BNF grammar.
  I.bnfTokenize = function (bnf) {
    var tokens = bnf.match(/(#[^#]*#)|([$][^$]+[$])|"([#\$\?\*\+\(\)\{\}\[\]\|])"|(\n)|([^\s]+)/gm);
    while (tokens[0] == "\n") {
      tokens = tokens.slice(1);
    }
    while (_.last(tokens) == "\n") {
      tokens = _.initial(tokens);
    }
    return tokens;
  };

  // Converts BNF grammar (in text) into a UxADT grammar.
  I.bnfToUxadt = function (bnf) {
    var tokens = I.bnfTokenize(bnf);
    var pt = I.parse(I.genGrammar, tokens);
    if (pt != null) {
      if (pt[1].length == 0) {
        var ux = I.bnfTreeToUxadt(pt[0]);
        return ux;
      } else {
        return "Syntax error, BNF could not be parsed.";
      }
    }
    return "Syntax error, BNF could not be parsed.";
  };

  // Converts a BNF parse tree and transforms it into UxADT.
  I.bnfTreeToUxadt = function (bnftree) {
    var ps = bnftree["Grammar"];
    var ux = [];

    // Helper function to convert expressions into UxADT.
    var toUxadtSeq = function (es) {
      var cs = [];
      var key = _.keys(es)[0];
      var value = es[key];
      if (typeof value[0] == "string") value = value[0];
      switch (key) {
        case "Comment":
          cs = I.Comment(value.substr(1, value.length-1));
          break;
        case "Terminal":
          if (value.length == 3 && value.charAt(0) == "\"" && value.charAt(2) == "\"") {
            value = value.charAt(1);
          }
          cs = I.Terminal(value);
          break;
        case "Nonterminal":
          cs = I.Nonterminal(value.substr(1));
          break;
        case "EmptyString":
          cs = I.Terminal("\"\""); 
          break;
        case "RegExpr":
          cs = I.RegExpr(value.substr(1, value.length-2));
          break;
        case "One":
          var m = [];
          for (var i = 0; i < value.length; i++) {
            m.push(toUxadtSeq(value[i]));
          }
          cs = I.One(m);
          break;
        case "May":
          var m = [];
          for (var i = 0; i < value.length; i++) {
            m.push(toUxadtSeq(value[i]));
          }
          cs = I.May(m);
          break;
        case "Many":
          var m = [];
          for (var i = 0; i < value.length; i++) {
            m.push(toUxadtSeq(value[i]));
          }
          cs = I.Many(m);
          break;
        case "MayMany":
          var m = [];
          for (var i = 0; i < value.length; i++) {
            m.push(toUxadtSeq(value[i]));
          }
          cs = I.MayMany(m);
          break;
        case "Group":
          for (var i = 0; i < value.length; i++) {
            cs.push(toUxadtSeq(value[i]));
          }
          break;
      }
      return cs;
    };

    for (var i = 0; i < _.size(ps); i++) {
      var p = ps[i]["Production"];
      var prodName = p[0];
      var cs = _.rest(p);
      var cb = [];
      for (var j = 0; j < _.size(cs); j++)  {
        var label = cs[j]["Choice"][0];
        var c = _.rest(cs[j]["Choice"]);
        var seq = [];
        if (label == "None") label = null; // Choice does not have a label
        for (var k = 0; k < _.size(c); k++) {
          var r = toUxadtSeq(c[k]);
          if (_.isArray(r)) {
            seq.extend(r);
          } else seq.push(r);
        }
        cb.push(I.Choice(label, I.AssocNone(), seq));
      }
      ux.push(I.Production(prodName, [I.Choices(cb)]));
    }
    return I.Grammar(ux);
  };


  ///////////////////////////////////////////////////////////////////
  // Useful helper functions.

  // Extend an array with another array.
  Array.prototype.extend = function (other_array) {
    if (_.isArray(other_array)) {
      other_array.forEach(function (v) { this.push(v); }, this);
    } else {
      console.log("Input is not an array.");
    }
  };

  if(!window.imparse) {
    window.imparse = imparse;
  }
})(_, uxadt);

/* eof */
