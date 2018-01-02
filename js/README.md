# imparse

Lightweight infinite-lookahead parser generator that supports basic grammars defined in a JSON format. More information and interactive examples are available at [imparse.org](http://imparse.org).

[![npm version and link.](https://badge.fury.io/js/imparse.svg)](https://badge.fury.io/js/imparse)

This library makes it possible to rapidly assemble and deploy a parser for a simple language. It is intended primarily for languages that have an [LL grammar](https://en.wikipedia.org/wiki/LL_grammar).

## Usage

### Representation of Grammars

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

* a *grammar* consists of an array of production rules;
* each *production rule* is represented by an object that maps the name of its non-terminal to an array of possible cases;
* each *case* is represented by an object that maps a case name to a sequence of terminals and non-terminals; and
* each *entry* in a case sequence can be a terminal (represented as a string), non-terminal (represented by a singleton array with a non-terminal string), or regular expression (represented as an object with a single key `"RegExp"` that maps to the actual regular expression string).

Note that the *case name* (i.e., the sole key in each case object) is used within any abstract syntax tree node constructed according to that case. For example, if a token sequence or string is parsed successfully according to the case sequence `{"Add": [["Factor"], "+", ["Term"]]}`, then the resulting abstract syntax tree will be of the form `{"Add":[...]}`.

### Basic Parsing

It is possible to parse a string according to the grammar in the following way:
```javascript
imparse.parse(grammar, '1*2 + 3*4')
```
The above yields the following abstract syntax tree:
```javascript
{"Add":[
  {"Mul":[{"Num":["1"]},{"Num":["2"]}]},
  {"Mul":[{"Num":["3"]},{"Num":["4"]}]}
]}
```
