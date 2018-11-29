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

function mem(_hash, key) {
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

function find_opt(_hash, key) {
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

function add(hash, key, value) {
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
              add(left, key, value),
              right
            ];
    } else {
      return /* Branch */[
              /* tuple */[
                k,
                v
              ],
              left,
              add(right, key, value)
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

function replace(hash, key, value) {
  if (hash) {
    var right = hash[2];
    var left = hash[1];
    var match = hash[0];
    var k = match[0];
    if (Caml_obj.caml_equal(k, key)) {
      return /* Branch */[
              /* tuple */[
                key,
                value
              ],
              left,
              right
            ];
    } else {
      var v = match[1];
      if (Caml_obj.caml_greaterthan(k, key)) {
        return /* Branch */[
                /* tuple */[
                  k,
                  v
                ],
                replace(left, key, value),
                right
              ];
      } else {
        return /* Branch */[
                /* tuple */[
                  k,
                  v
                ],
                left,
                replace(right, key, value)
              ];
      }
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

function fold_left(_hash, _value, func) {
  while(true) {
    var value = _value;
    var hash = _hash;
    if (hash) {
      var match = hash[0];
      var result = Curry._3(func, value, match[0], match[1]);
      var temp = fold_left(hash[1], result, func);
      _value = temp;
      _hash = hash[2];
      continue ;
    } else {
      return value;
    }
  };
}

function pairs(hash) {
  return List.rev(fold_left(hash, /* [] */0, (function (memo, k, v) {
                    return /* :: */[
                            /* tuple */[
                              k,
                              v
                            ],
                            memo
                          ];
                  })));
}

function keys(hash) {
  return List.rev(fold_left(hash, /* [] */0, (function (memo, k, _) {
                    return /* :: */[
                            k,
                            memo
                          ];
                  })));
}

function values(hash) {
  return List.rev(fold_left(hash, /* [] */0, (function (memo, _, v) {
                    return /* :: */[
                            v,
                            memo
                          ];
                  })));
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
/* No side effect */
