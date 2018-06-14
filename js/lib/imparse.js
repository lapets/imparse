/**
 * imparse.js
 * http://imparse.org
 *
 * Lightweight infinite-lookahead parser generator that supports basic grammars
 * defined in a JSON format.
 *
 * @namespace imparse
 */

(function (imparse) {

  /**
   * Tokenize a string according to a grammer.
   * @memberof imparse
   * @param {Object} grammar - a grammar represented as an object.
   * @param {string} s - the string to tokenize.
   * @return {string[]} the array of tokens obtained from the input string.
   */
  imparse.tokenize = function (grammar, s) {
    // Extract terminals from grammar.
    var terminals = [];
    for (var i = 0; i < grammar.length; i++) {
      for (var nt in grammar[i]) {
        for (var j = 0; j < grammar[i][nt].length; j++) {
          for (var con in grammar[i][nt][j]) {
            var seq = grammar[i][nt][j][con];
            for (var k = 0; k < seq.length; k++) {
              if (!(seq[k] instanceof Array)) {
                terminals.push(seq[k]);
              }
            }
          }
        }
      }
    };

    var tokens = [], row = 0, col = 0;
    while (s.length > 0) {
      while (s[0] == " " || s[0] == "\n") {
        if (s[0] == "\n") {
          row++;
          col = 0;
        } else {
          col++;
        }
        s = s.slice(1);
      }
      var m = [""], len = 0;
      for (var i = 0; i < terminals.length; i++) {
        if (terminals[i] instanceof Object && 'RegExp' in terminals[i]) {
          var c = s.match(new RegExp('^' + terminals[i]['RegExp']));
          m = (c != null && c[0].length > m[0].length) ? c : m;
        } else {
          var c = s.substr(0,terminals[i].length);
          m = (c == terminals[i]) ? [c] : m;
        }
      }
      if (m[0].length > 0) {
        s = s.slice(m[0].length);
        tokens.push({'str':m[0], 'row':row, 'col':col});
        col += m[0].length;
      } else {
        if (s.length > 0)
          throw new Error("Did not tokenize entire string.");
        break;
      }
    }
    return tokens;
  };

  /**
   * Show a token sequence as a string.
   * @memberof imparse
   * @param {string[]} ts - an array representing a token sequence.
   * @return {string} a string representation of the token sequence.
   */
  imparse.show_tokens = function (ts) {
    var s = "", row = 0, col = 0;
    for (var i = 0; i < ts.length; i++) {
      while (row < ts[i].row) { s += "\n"; row++; col = 0; }
      while (col < ts[i].col) { s += " "; col++; }
      s += ts[i].str;
      col += ts[i].str.length;
    }
    return s;
  };

  /**
   * Parse a token sequence into an AST according to grammar.
   * @memberof imparse
   * @param {Object} grammar - a grammar represented as an object.
   * @param {string[]} ts_original - an array representing a token sequence.
   * @param {string} nonterm - root production rule's non-terminal.
   * @return {Object} an abstract syntax tree (AST) of nested objects.
   * @throws error if grammar object is not constructed in a valid way.
   */
  imparse.parse_tokens = function (grammar, ts_original, nonterm) {
    // Find the appropriate produciton.
    for (var i = 0; i < grammar.length; i++) {
      if (nonterm in grammar[i]) {
        var longest = {"result":null, "len":0};

        // For each option in the production.
        for (var j = 0; j < grammar[i][nonterm].length; j++) {
          var ts = ts_original, seq = grammar[i][nonterm][j];
          for (var con in seq) { // Unwrap singleton JSON object.
            var success = true, subtrees = [];
            for (var k = 0; k < seq[con].length; k++) { // Iterate over sequence entries.
              if (ts.length == 0) { // This option failed, but others may succeed.
                success = false;
                break;
              }
              // Handle each type of sequence entry that can appear in the sequence.
              var entry = seq[con][k];
              if (entry instanceof Array) {
                var result = imparse.parse_tokens(grammar, ts, entry[0]);
                if (result instanceof Array && result.length == 2) {
                  subtrees.push(result[0]);
                  ts = result[1];
                } else {
                  success = false;
                  break; // This sequence did not succeed.
                }
              } else if (entry instanceof Object && 'RegExp' in entry) {
                var c = ts[0].str.match(new RegExp('^' + entry['RegExp']));
                if (c != null && c[0].length == ts[0].str.length) {
                  subtrees.push(ts[0].str);
                  ts = ts.slice(1);
                } else {
                  success = false;
                  break;
                }
              } else {
                if (ts[0].str == entry) {
                  ts = ts.slice(1);
                } else {
                  success = false;
                  break;
                }
              }
            } // for each entry in the sequence

            if (success) {
              var ts_consumed = ts_original.length - ts.length;
              if (con.length > 0) {
                if (ts_consumed > longest.len) {
                  var o = {};
                  o[con] = subtrees
                  longest = {"result":[o, ts], "len":ts_consumed};
                }
              } else { // Pass-through option with only one subtree.
                if (subtrees.length > 1)
                  throw Error("Pass-through case sequence should only have one sequence entry.");
                if (ts_consumed > longest.len)
                  longest = {"result":[subtrees[0], ts], "len":ts_consumed};
              }
            } // if tokens parsed with option sequence successfully

          } // unwrap JSON object for constructor and sequence
        } // for each possible sequence under the non-terminal

        return longest.result; // First result that consumed as many tokens as any other.

      } // if production is the one specified by argument
    } // for each production in grammar
  };

  /**
   * Tokenize and parse a string into an AST according to a grammar.
   * @memberof imparse
   * @param {Object} grammar - a grammar represented as an object.
   * @param {string} s - a string to tokenize and parse.
   * @param {string} nonterm - root production rule's non-terminal.
   * @return {Object} an abstract syntax tree (AST) of nested objects.
   * @throws error if grammar object is not constructed in a valid way.
   */
  imparse.parse = function (grammar, s) {
    if (grammar.length > 0) {
      for (var nonterm in grammar[0]) {
        var tokens = imparse.tokenize(grammar, s);
        var tree_tokens = imparse.parse_tokens(grammar, tokens, nonterm);
        return (tree_tokens != null) ? tree_tokens[0] : null; // Return only the tree.
      }
    }
    throw Error("Cannot use the supplied grammar object.");
  };

})(typeof exports !== 'undefined' ? exports : (this.imparse = {}));
