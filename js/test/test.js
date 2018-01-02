var assert = require('assert');
var expect = require('chai').expect;
var imparse = require('../lib/imparse');

describe('imparse', function() {

  var graArith = [ 
    {"Term": [
      {"Add": [["Factor"], "+", ["Term"]]},
      {"": [["Factor"]]}
    ]},
    {"Factor": [
      {"Mul": [["Atom"], "*", ["Factor"]]},
      {"": [["Atom"]]}
    ]},
    {"Atom": [
      {"Num": [{"RegExp":"[0-9]+"}]}
    ]}
  ];

  describe('#parse()', function () { 
    it('parse', function() {
      assert.equal(JSON.stringify(imparse.parse(graArith, '1+2')), '{"Add":[{"Num":["1"]},{"Num":["2"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graArith, '1+2+3')), '{"Add":[{"Num":["1"]},{"Add":[{"Num":["2"]},{"Num":["3"]}]}]}');
      assert.equal(JSON.stringify(imparse.parse(graArith, '1*2 + 3*4')), '{"Add":[{"Mul":[{"Num":["1"]},{"Num":["2"]}]},{"Mul":[{"Num":["3"]},{"Num":["4"]}]}]}');
    });
  });
});
