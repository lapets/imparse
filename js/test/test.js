var assert = require('assert');
var expect = require('chai').expect;
var imparse = require('../lib/imparse');

describe('imparse', function() {
  describe('#parse()', function () {
    var graLiterals = [
      {"Stmt": [
        {"Foo": ["foo"]},
        {"BarBaz": ["bar", "baz"]}
      ]}
    ];
    it('literals', function() {
      assert.equal(JSON.stringify(imparse.parse(graLiterals, 'foo')), '{"result":{"Foo":[]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graLiterals, 'bar baz')), '{"result":{"BarBaz":[]},"success":1}');
    });

    var graList = [
      {"List": [
        {"Cons": ["()", ":", ["List"]]},
        {"Nil": ["[]"]}
      ]}
    ];
    it('recursion', function() {
      assert.equal(JSON.stringify(imparse.parse(graList, '[]')), '{"result":{"Nil":[]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graList, '():[]')), '{"result":{"Cons":[{"Nil":[]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graList, '():():[]')), '{"result":{"Cons":[{"Cons":[{"Nil":[]}]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graList, '():():():[]')), '{"result":{"Cons":[{"Cons":[{"Cons":[{"Nil":[]}]}]}]},"success":1}');
    });

    var graBacktrackLiterals = [
      {"Seq": [
        {"ABC": ["a", "b", "c"]},
        {"ABCD": ["a", "b", "c", "d"]},
        {"ABCDE": ["a", "b", "c", "d", "e"]}
      ]}
    ];
    it('backtrack-literals', function() {
      assert.equal(JSON.stringify(imparse.parse(graBacktrackLiterals, 'abcd')), '{"result":{"ABCD":[]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graBacktrackLiterals, 'abcde')), '{"result":{"ABCDE":[]},"success":1}');
    });

    var graBacktrackRecursion = [
      {"Alpha": [
        {"One": [["Beta"], "end"]},
        {"Two": [["Beta"], ["Beta"], "end"]}
      ]},
      {"Beta": [
        {"Beta": ["beta"]}
      ]}
    ];
    it('backtrack-recursion', function() {
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursion, 'beta end')), '{"result":{"One":[{"Beta":[]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursion, 'beta beta end')), '{"result":{"Two":[{"Beta":[]},{"Beta":[]}]},"success":1}');
    });

    var graBacktrackRecursionPassThrough = [
      {"Alpha": [
        {"": [["Beta"]]},
        {"Two": ["two"]}
      ]},
      {"Beta": [
        {"One": ["one"]}
      ]}
    ];
    it('backtrack-recursion-pass-through', function() {
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursionPassThrough, 'one')), '{"result":{"One":[]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursionPassThrough, 'two')), '{"result":{"Two":[]},"success":1}');
    });

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
    it('arithmetic', function() {
      assert.equal(JSON.stringify(imparse.parse(graArith, '1+2')),
        '{"result":{"Add":[{"Num":["1"]},{"Num":["2"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graArith, '1+2+3')),
        '{"result":{"Add":[{"Num":["1"]},{"Add":[{"Num":["2"]},{"Num":["3"]}]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graArith, '1*2 + 3*4')),
        '{"result":{"Add":[{"Mul":[{"Num":["1"]},{"Num":["2"]}]},{"Mul":[{"Num":["3"]},{"Num":["4"]}]}]},"success":1}');
    });

    var graPolynomials = [
      {"Term": [
        {"Add": [["Factor"], "+", ["Term"]]},
        {"Sub": [["Factor"], "-", ["Term"]]},
        {"": [["Factor"]]}
      ]},
      {"Factor": [
        {"Mul": [["Atom"], "*", ["Factor"]]},
        {"Mul": [["Atom"], ["Factor"]]},
        {"": [["Atom"]]}
      ]},
      {"Atom": [
        {"": ["(", ["Term"], ")"]},
        {"Num": [{"RegExp":"[0-9]+"}]},
        {"Var": [{"RegExp":"[a-z]"}]}
      ]}
    ];
    it('polynomials', function() {
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '1+2')),
        '{"result":{"Add":[{"Num":["1"]},{"Num":["2"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '1+2+3')),
        '{"result":{"Add":[{"Num":["1"]},{"Add":[{"Num":["2"]},{"Num":["3"]}]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(1+2)+3')),
        '{"result":{"Add":[{"Add":[{"Num":["1"]},{"Num":["2"]}]},{"Num":["3"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2+x)')),
        '{"result":{"Add":[{"Num":["2"]},{"Var":["x"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2*x)')),
        '{"result":{"Mul":[{"Num":["2"]},{"Var":["x"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2x)')),
        '{"result":{"Mul":[{"Num":["2"]},{"Var":["x"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, 'y*(2*x)')),
        '{"result":{"Mul":[{"Var":["y"]},{"Mul":[{"Num":["2"]},{"Var":["x"]}]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, 'y(2*x)')),
        '{"result":{"Mul":[{"Var":["y"]},{"Mul":[{"Num":["2"]},{"Var":["x"]}]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2*x)*y')),
        '{"result":{"Mul":[{"Mul":[{"Num":["2"]},{"Var":["x"]}]},{"Var":["y"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2*x)y')),
        '{"result":{"Mul":[{"Mul":[{"Num":["2"]},{"Var":["x"]}]},{"Var":["y"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '((2*x)*y)+z')),
        '{"result":{"Add":[{"Mul":[{"Mul":[{"Num":["2"]},{"Var":["x"]}]},{"Var":["y"]}]},{"Var":["z"]}]},"success":1}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '1*2 + 3*4')),
        '{"result":{"Add":[{"Mul":[{"Num":["1"]},{"Num":["2"]}]},{"Mul":[{"Num":["3"]},{"Num":["4"]}]}]},"success":1}');
    });
  });
});
