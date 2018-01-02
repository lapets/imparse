# imparse

Lightweight infinite-lookahead parser generator that supports basic grammars defined in a JSON format. More information and interactive examples are available at [imparse.org](http://imparse.org).

[![npm version and link.](https://badge.fury.io/js/imparse.svg)](https://badge.fury.io/js/imparse)

This library makes it possible to rapidly assemble and deploy a parser for a simple language. It is intended primarily for languages that have an [LL grammar](https://en.wikipedia.org/wiki/LL_grammar).

## Usage

Suppose we want to represent a grammar of basic arithmetic expressions in the following way (assuming that operators will associate to the right):

```javascript
var grammar = [ 
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
```

It is assumed that grammars are represented as nested objects according to the following conventions:
* a grammar consists of an array of production rules;
* each production rule is represented by an object that maps the name of its non-terminal to an array of possible cases; and
* each case is represented by an object that maps a case name to a sequence of terminals and non-terminals.
Note that the case name is used within the abstract syntax tree constructed according to that case.

It is possible to parse a string according to the grammar in the following way:

```javascript
imparse.parse(graArith, '1*2 + 3*4')
```

The above yields the following abstract syntax tree:
```javascript
{"Add":[
  {"Mul":[{"Num":["1"]},{"Num":["2"]}]},
  {"Mul":[{"Num":["3"]},{"Num":["4"]}]}
]}
```
