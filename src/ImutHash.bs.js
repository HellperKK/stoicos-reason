// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");

function singleton(key, value) {
  return /* Branch */[
          /* tuple */[
            key,
            value
          ],
          /* Leaf */0,
          /* Leaf */0
        ];
}

function mem(key, _hash) {
  while(true) {
    var hash = _hash;
    if (hash) {
      var k = hash[0][0];
      if (Caml_obj.caml_equal(k, key)) {
        return true;
      } else if (Caml_obj.caml_greaterthan(k, key)) {
        _hash = hash[1];
        continue ;
      } else {
        _hash = hash[2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function find_opt(key, _hash) {
  while(true) {
    var hash = _hash;
    if (hash) {
      var match = hash[0];
      var k = match[0];
      if (Caml_obj.caml_equal(k, key)) {
        return Js_primitive.some(match[1]);
      } else if (Caml_obj.caml_greaterthan(k, key)) {
        _hash = hash[1];
        continue ;
      } else {
        _hash = hash[2];
        continue ;
      }
    } else {
      return undefined;
    }
  };
}

function add(key, value, hash) {
  if (hash) {
    var right = hash[2];
    var left = hash[1];
    var match = hash[0];
    var v = match[1];
    var k = match[0];
    if (Caml_obj.caml_equal(k, key)) {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              left,
              right
            ];
    } else if (Caml_obj.caml_greaterthan(k, key)) {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              add(key, value, left),
              right
            ];
    } else {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              left,
              add(key, value, right)
            ];
    }
  } else {
    return /* Branch */[
            /* tuple */[
              key,
              value
            ],
            /* Leaf */0,
            /* Leaf */0
          ];
  }
}

function replace(key, value, hash) {
  if (hash) {
    var right = hash[2];
    var left = hash[1];
    var match = hash[0];
    var v = match[1];
    var k = match[0];
    if (Caml_obj.caml_equal(k, key)) {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              left,
              right
            ];
    } else if (Caml_obj.caml_greaterthan(k, key)) {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              replace(key, value, left),
              right
            ];
    } else {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              left,
              replace(key, value, right)
            ];
    }
  } else {
    return /* Branch */[
            /* tuple */[
              key,
              value
            ],
            /* Leaf */0,
            /* Leaf */0
          ];
  }
}

function fold_left(_value, func, _hash) {
  while(true) {
    var hash = _hash;
    var value = _value;
    if (hash) {
      var match = hash[0];
      var result = Curry._3(func, value, match[0], match[1]);
      var temp = fold_left(result, func, hash[1]);
      _hash = hash[2];
      _value = temp;
      continue ;
    } else {
      return value;
    }
  };
}

function pairs(hash) {
  return List.rev(fold_left(/* [] */0, (function (memo, k, v) {
                    return /* :: */[
                            /* tuple */[
                              k,
                              v
                            ],
                            memo
                          ];
                  }), hash));
}

function keys(hash) {
  return List.rev(fold_left(/* [] */0, (function (memo, k, _) {
                    return /* :: */[
                            k,
                            memo
                          ];
                  }), hash));
}

function values(hash) {
  return List.rev(fold_left(/* [] */0, (function (memo, _, v) {
                    return /* :: */[
                            v,
                            memo
                          ];
                  }), hash));
}

function iter(fn, hash) {
  return fold_left(/* () */0, (function (_, k, v) {
                return Curry._2(fn, k, v);
              }), hash);
}

function test() {
  var test_val = add("man", 4, add("doe", 3, add("john", 2, add("hello", 1, /* Leaf */0))));
  iter((function (k, _) {
          console.log(k);
          return /* () */0;
        }), test_val);
  iter((function (_, v) {
          console.log(v);
          return /* () */0;
        }), test_val);
  console.log(find_opt("john", test_val));
  console.log(pairs(test_val));
  console.log(keys(test_val));
  console.log(values(test_val));
  console.log(mem("hello", test_val));
  console.log(mem("hellos", test_val));
  return /* () */0;
}

var empty = /* Leaf */0;

exports.empty = empty;
exports.singleton = singleton;
exports.mem = mem;
exports.find_opt = find_opt;
exports.add = add;
exports.replace = replace;
exports.fold_left = fold_left;
exports.pairs = pairs;
exports.keys = keys;
exports.values = values;
exports.iter = iter;
exports.test = test;
/* No side effect */
