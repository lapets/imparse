var Imparse = (function(uxadt, Informl){
  var Imparse = {};
  
  
  Imparse.tokenize = function (parser, raw) {
    if (__iml0 = uxadt.M(parser, uxadt.C("Parser", [uxadt.C("Productions", [uxadt.V("ps")]), uxadt.C("Terminals", [uxadt.V("ts")])]))) {var ps = __iml0["ps"];var ts = __iml0["ts"];
      var tokens = [];
      while (Informl.size(raw) > 0) {
        raw = Informl.__trim_whitespace_from_X(raw);
        var match = false;
        var longest = null;
        var __iml1 = ts;for (var __iml2 = 0; __iml2 < __iml1.length; __iml2++) {var t = __iml1[__iml2];
          if (__iml3 = uxadt.M(t, uxadt.C("Terminal", [uxadt.V("t")]))) {var t = __iml3["t"];
            if (Informl.__X_is_prefix_of_X(t, raw)) {
              raw = Informl.__suffix_of_X_after_index_X(raw, Informl.size(t));
              match = true;
              longest = t;
            }
          }
        }
        if (match == false) {
          break;
        }
        if (match == true) {
          tokens = Informl.plus(tokens, [longest]);
        }
      }
      return tokens;
    }
    return null;
  }
  
  Imparse.parse = function (parser, tokens) {
    return null;
  }
  
  return Imparse;
}(uxadt, Informl));