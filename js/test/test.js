var assert = require('assert');
var expect = require('chai').expect;
var imparse = require('../src/imparse');

describe('imparse', function() {
  describe('#parse()', function () {
    var graLiterals = [
      {"Stmt": [
        {"Foo": ["foo"]},
        {"BarBaz": ["bar", "baz"]}
      ]}
    ];
    it('literals', function() {
      assert.equal(JSON.stringify(imparse.parse(graLiterals, 'foo')), '{"Foo":[]}');
      assert.equal(JSON.stringify(imparse.parse(graLiterals, 'bar baz')), '{"BarBaz":[]}');
    });

    var graList = [
      {"List": [
        {"Cons": ["()", ":", ["List"]]},
        {"Nil": ["[]"]}
      ]}
    ];
    it('recursion', function() {
      assert.equal(JSON.stringify(imparse.parse(graList, '[]')), '{"Nil":[]}');
      assert.equal(JSON.stringify(imparse.parse(graList, '():[]')), '{"Cons":[{"Nil":[]}]}');
      assert.equal(JSON.stringify(imparse.parse(graList, '():():[]')), '{"Cons":[{"Cons":[{"Nil":[]}]}]}');
      assert.equal(JSON.stringify(imparse.parse(graList, '():():():[]')), '{"Cons":[{"Cons":[{"Cons":[{"Nil":[]}]}]}]}');
    });

    var graBacktrackLiterals = [
      {"Seq": [
        {"ABC": ["a", "b", "c"]},
        {"ABCD": ["a", "b", "c", "d"]},
        {"ABCDE": ["a", "b", "c", "d", "e"]}
      ]}
    ];
    it('backtrack-literals', function() {
      assert.equal(JSON.stringify(imparse.parse(graBacktrackLiterals, 'abcd')), '{"ABCD":[]}');
      assert.equal(JSON.stringify(imparse.parse(graBacktrackLiterals, 'abcde')), '{"ABCDE":[]}');
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
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursion, 'beta end')), '{"One":[{"Beta":[]}]}');
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursion, 'beta beta end')), '{"Two":[{"Beta":[]},{"Beta":[]}]}');
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
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursionPassThrough, 'one')), '{"One":[]}');
      assert.equal(JSON.stringify(imparse.parse(graBacktrackRecursionPassThrough, 'two')), '{"Two":[]}');
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
        '{"Add":[{"Num":["1"]},{"Num":["2"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graArith, '1+2+3')), 
        '{"Add":[{"Num":["1"]},{"Add":[{"Num":["2"]},{"Num":["3"]}]}]}');
      assert.equal(JSON.stringify(imparse.parse(graArith, '1*2 + 3*4')), 
        '{"Add":[{"Mul":[{"Num":["1"]},{"Num":["2"]}]},{"Mul":[{"Num":["3"]},{"Num":["4"]}]}]}');
    });

    var graPolynomials = [
      {"Term": [
        {"Add": [["Factor"], "+", ["Term"]]},
        {"Sub": [["Factor"], "-", ["Term"]]},
        {"": [["Factor"]]}
      ]},
      {"Factor": [
        {"Mul": [["Atom"], "*", ["Factor"]]},
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
        '{"Add":[{"Num":["1"]},{"Num":["2"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '1+2+3')),
        '{"Add":[{"Num":["1"]},{"Add":[{"Num":["2"]},{"Num":["3"]}]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(1+2)+3')),
        '{"Add":[{"Add":[{"Num":["1"]},{"Num":["2"]}]},{"Num":["3"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2+x)')),
        '{"Add":[{"Num":["2"]},{"Var":["x"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2*x)')),
        '{"Mul":[{"Num":["2"]},{"Var":["x"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, 'y*(2*x)')),
        '{"Mul":[{"Var":["y"]},{"Mul":[{"Num":["2"]},{"Var":["x"]}]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '(2*x)*y')),
        '{"Mul":[{"Mul":[{"Num":["2"]},{"Var":["x"]}]},{"Var":["y"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '((2*x)*y)+z')),
        '{"Add":[{"Mul":[{"Mul":[{"Num":["2"]},{"Var":["x"]}]},{"Var":["y"]}]},{"Var":["z"]}]}');
      assert.equal(JSON.stringify(imparse.parse(graPolynomials, '1*2 + 3*4')),
        '{"Add":[{"Mul":[{"Num":["1"]},{"Num":["2"]}]},{"Mul":[{"Num":["3"]},{"Num":["4"]}]}]}');
    });
  });
});
