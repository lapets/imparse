var Imparse = (function(uxadt, Informl){
  var Imparse = {};
  
  
  Imparse.tokenize = function (parser, rawStr) {
    if (__iml0 = uxadt.M(parser, uxadt.C("Parser", [uxadt.C("Productions", [uxadt.V("ps")]), uxadt.C("Terminals", [uxadt.V("ts")])]))) {var ps = __iml0["ps"];var ts = __iml0["ts"];
      return Informl.size(ts);
    }
    return uxadt.C("Nothing", []);
  }
  
  return Imparse;
}(uxadt, Informl));