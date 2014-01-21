/********************************************************************
** 
** richreports.js
**
** A library that supports the construction of interactive, annotated
** reports that summarize the results of static analysis algorithms
** applied to abstract syntax trees.
**
**   Web:     richreports.org
**   Version: 0.0.0.2
**
*/

(function (_, uxadt, richreports) {

  // Synonyms.
  var U = uxadt;
  var RR = richreports;

  ///////////////////////////////////////////////////////////////////
  // Data type definitions.

  U.define(RR, {
      Just: [_],
      Nothing: []
    });

  U.define(RR, { Choices: [_] });
  U.define(RR, { Choice: [_, _, _] });

  U.define(RR, {
      Nonterminal: [_],
      RegExpr: [_],
      Terminal: [_]
    });

  ///////////////////////////////////////////////////////////////////
  // Functionality.   

  richreports.html = function(r) {
    return "";
  }

})(_, uxadt, this.richreports = {});

/* eof */