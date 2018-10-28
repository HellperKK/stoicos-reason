// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function list_slice(min, max, liste) {
  var aux = function (_acc, _i) {
    while(true) {
      var i = _i;
      var acc = _acc;
      var match = i === max;
      if (match) {
        return acc;
      } else {
        _i = i + 1 | 0;
        _acc = /* :: */[
          List.nth(liste, i),
          acc
        ];
        continue ;
      }
    };
  };
  return List.rev(aux(/* [] */0, min));
}

function string_slice(min, max, chaine) {
  var size = max - min | 0;
  return $$String.sub(chaine, min, size);
}

function super_int_of_string(chaine) {
  try {
    return Caml_format.caml_int_of_string(chaine);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      if (exn[1] === "int_of_string") {
        return 0;
      } else {
        throw exn;
      }
    } else {
      throw exn;
    }
  }
}

function super_float_of_string(chaine) {
  try {
    return Caml_format.caml_float_of_string(chaine);
  }
  catch (raw_exn){
    var exn = Js_exn.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      if (exn[1] === "float_of_string") {
        return 0.0;
      } else {
        throw exn;
      }
    } else {
      throw exn;
    }
  }
}

exports.list_slice = list_slice;
exports.string_slice = string_slice;
exports.super_int_of_string = super_int_of_string;
exports.super_float_of_string = super_float_of_string;
/* No side effect */
