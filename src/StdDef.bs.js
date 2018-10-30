// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Type$ReactTemplate = require("./Type.bs.js");

Type$ReactTemplate.set_value("print", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var chaines = List.map(Type$ReactTemplate.to_string, tokens);
                var chaine = $$String.concat(" ", chaines);
                Type$ReactTemplate.sortie[0] = Type$ReactTemplate.sortie[0] + chaine;
                return /* Chaine */Block.__(3, [chaine]);
              })])]));

Type$ReactTemplate.set_value("=", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var name = Type$ReactTemplate.to_sym(List.nth(tokens, 0));
                var value = List.nth(tokens, 1);
                Type$ReactTemplate.set_value(name, value);
                return /* Unit */0;
              })])]));

Type$ReactTemplate.set_value("if", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var bool = Type$ReactTemplate.to_bool(List.nth(tokens, 0));
                var bloc = Type$ReactTemplate.to_block(List.nth(tokens, 1));
                var blocb = Type$ReactTemplate.to_block(List.nth(tokens, 2));
                if (bool) {
                  return Type$ReactTemplate.tok_calc(bloc);
                } else {
                  return Type$ReactTemplate.tok_calc(blocb);
                }
              })])]));

Type$ReactTemplate.set_value("not", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var bool = Type$ReactTemplate.to_bool(List.nth(tokens, 0));
                return /* Booleen */Block.__(6, [!bool]);
              })])]));

Type$ReactTemplate.set_value("or", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          if (prim) {
                            return true;
                          } else {
                            return prim$1;
                          }
                        }), Type$ReactTemplate.to_bool(tokens[0]), List.map(Type$ReactTemplate.to_bool, tokens[1]));
                  return /* Booleen */Block.__(6, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("and", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          if (prim) {
                            return prim$1;
                          } else {
                            return false;
                          }
                        }), Type$ReactTemplate.to_bool(tokens[0]), List.map(Type$ReactTemplate.to_bool, tokens[1]));
                  return /* Booleen */Block.__(6, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("^", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var chaines = List.map(Type$ReactTemplate.to_string, tokens);
                var chaine = $$String.concat("", chaines);
                return /* Chaine */Block.__(3, [chaine]);
              })])]));

Type$ReactTemplate.set_value("+", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left((function (prim, prim$1) {
                        return prim + prim$1 | 0;
                      }), 0, List.map(Type$ReactTemplate.to_int, tokens));
                return /* Entier */Block.__(0, [result]);
              })])]));

Type$ReactTemplate.set_value("*", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left(Caml_int32.imul, 1, List.map(Type$ReactTemplate.to_int, tokens));
                return /* Entier */Block.__(0, [result]);
              })])]));

Type$ReactTemplate.set_value("-", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim - prim$1 | 0;
                        }), Type$ReactTemplate.to_int(tokens[0]), List.map(Type$ReactTemplate.to_int, tokens[1]));
                  return /* Entier */Block.__(0, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("/", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left(Caml_int32.div, Type$ReactTemplate.to_int(tokens[0]), List.map(Type$ReactTemplate.to_int, tokens[1]));
                  return /* Entier */Block.__(0, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("%", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left(Caml_int32.mod_, Type$ReactTemplate.to_int(tokens[0]), List.map(Type$ReactTemplate.to_int, tokens[1]));
                  return /* Entier */Block.__(0, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("+.", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left((function (prim, prim$1) {
                        return prim + prim$1;
                      }), 0, List.map(Type$ReactTemplate.to_float, tokens));
                return /* Flottant */Block.__(1, [result]);
              })])]));

Type$ReactTemplate.set_value("*.", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left((function (prim, prim$1) {
                        return prim * prim$1;
                      }), 1.0, List.map(Type$ReactTemplate.to_float, tokens));
                return /* Flottant */Block.__(1, [result]);
              })])]));

Type$ReactTemplate.set_value("-.", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim - prim$1;
                        }), Type$ReactTemplate.to_float(tokens[0]), List.map(Type$ReactTemplate.to_float, tokens[1]));
                  return /* Flottant */Block.__(1, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("/.", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim / prim$1;
                        }), Type$ReactTemplate.to_float(tokens[0]), List.map(Type$ReactTemplate.to_float, tokens[1]));
                  return /* Flottant */Block.__(1, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

Type$ReactTemplate.set_value("%.", /* Fonction */Block.__(11, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim % prim$1;
                        }), Type$ReactTemplate.to_float(tokens[0]), List.map(Type$ReactTemplate.to_float, tokens[1]));
                  return /* Flottant */Block.__(1, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

var vars = Type$ReactTemplate.vars;

var get_value = Type$ReactTemplate.get_value;

var add_stack = Type$ReactTemplate.add_stack;

var remove_stack = Type$ReactTemplate.remove_stack;

var get_stack = Type$ReactTemplate.get_stack;

var set_value = Type$ReactTemplate.set_value;

var get_var = Type$ReactTemplate.get_var;

var sortie = Type$ReactTemplate.sortie;

var to_int = Type$ReactTemplate.to_int;

var to_float = Type$ReactTemplate.to_float;

var to_char = Type$ReactTemplate.to_char;

var to_string = Type$ReactTemplate.to_string;

var to_sym = Type$ReactTemplate.to_sym;

var to_bool = Type$ReactTemplate.to_bool;

var to_block = Type$ReactTemplate.to_block;

var to_function = Type$ReactTemplate.to_function;

var run = Type$ReactTemplate.run;

var tok_get = Type$ReactTemplate.tok_get;

var tok_calc = Type$ReactTemplate.tok_calc;

var run_fun = Type$ReactTemplate.run_fun;

exports.vars = vars;
exports.get_value = get_value;
exports.add_stack = add_stack;
exports.remove_stack = remove_stack;
exports.get_stack = get_stack;
exports.set_value = set_value;
exports.get_var = get_var;
exports.sortie = sortie;
exports.to_int = to_int;
exports.to_float = to_float;
exports.to_char = to_char;
exports.to_string = to_string;
exports.to_sym = to_sym;
exports.to_bool = to_bool;
exports.to_block = to_block;
exports.to_function = to_function;
exports.run = run;
exports.tok_get = tok_get;
exports.tok_calc = tok_calc;
exports.run_fun = run_fun;
/*  Not a pure module */
