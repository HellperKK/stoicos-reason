// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Utils$ReactTemplate = require("./Utils.bs.js");
var StdDef$ReactTemplate = require("./StdDef.bs.js");
var ImutHash$ReactTemplate = require("./ImutHash.bs.js");

function first_char_at(liste, i) {
  return Caml_string.get(List.nth(liste, i), 0);
}

function full_test(reg, chaine) {
  var match = reg.exec(chaine);
  if (match !== null) {
    var match$1 = Caml_array.caml_array_get(match, 0);
    if (match$1 == null) {
      return false;
    } else {
      return match$1 === chaine;
    }
  } else {
    return false;
  }
}

function find_next_char(car, chaine) {
  var _i = 1;
  while(true) {
    var i = _i;
    if (i >= (chaine.length - 1 | 0)) {
      return undefined;
    } else if (Caml_string.get(chaine, i) === car) {
      return i;
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

function find_matching_char(car, carb, chaine) {
  var _i = 1;
  var _compteur = 1;
  while(true) {
    var compteur = _compteur;
    var i = _i;
    if (i >= (chaine.length - 1 | 0)) {
      return undefined;
    } else if (compteur !== 0) {
      if (Caml_string.get(chaine, i) === car) {
        _compteur = compteur - 1 | 0;
        _i = i + 1 | 0;
        continue ;
      } else if (Caml_string.get(chaine, i) === carb) {
        _compteur = compteur + 1 | 0;
        _i = i + 1 | 0;
        continue ;
      } else {
        _i = i + 1 | 0;
        continue ;
      }
    } else {
      return i;
    }
  };
}

function decrement_opt(x) {
  if (x !== undefined) {
    return x - 1 | 0;
  }
  
}

function third_cut(chaine) {
  var aux = function (_acc, _chaineb) {
    while(true) {
      var chaineb = _chaineb;
      var acc = _acc;
      var chaineb$1 = $$String.trim(chaineb);
      if (chaineb$1 === "") {
        return acc;
      } else {
        var match = Caml_string.get(chaineb$1, 0);
        var milieu;
        if (match !== 91) {
          if (match >= 41) {
            milieu = match !== 123 ? decrement_opt(find_next_char(/* " " */32, chaineb$1)) : find_matching_char(/* "}" */125, /* "{" */123, chaineb$1);
          } else if (match >= 34) {
            switch (match - 34 | 0) {
              case 0 : 
                  milieu = find_next_char(/* "\"" */34, chaineb$1);
                  break;
              case 1 : 
              case 2 : 
              case 3 : 
              case 4 : 
                  milieu = decrement_opt(find_next_char(/* " " */32, chaineb$1));
                  break;
              case 5 : 
                  milieu = find_next_char(/* "'" */39, chaineb$1);
                  break;
              case 6 : 
                  milieu = find_matching_char(/* ")" */41, /* "(" */40, chaineb$1);
                  break;
              
            }
          } else {
            milieu = decrement_opt(find_next_char(/* " " */32, chaineb$1));
          }
        } else {
          milieu = find_matching_char(/* "]" */93, /* "[" */91, chaineb$1);
        }
        if (milieu !== undefined) {
          var x = milieu;
          var debut = $$String.trim(Utils$ReactTemplate.string_slice(0, x + 1 | 0, chaineb$1));
          var fin = $$String.trim(Utils$ReactTemplate.string_slice(x + 1 | 0, chaineb$1.length, chaineb$1));
          _chaineb = fin;
          _acc = /* :: */[
            debut,
            acc
          ];
          continue ;
        } else {
          return /* :: */[
                  chaineb$1,
                  acc
                ];
        }
      }
    };
  };
  return List.rev(aux(/* [] */0, chaine));
}

function to_token(chaine) {
  if (full_test((/[1-9]*[0-9]/), chaine)) {
    return /* Entier */Block.__(0, [Utils$ReactTemplate.super_int_of_string(chaine)]);
  } else if (full_test((/[1-9]*[0-9].[0-9]*/), chaine)) {
    return /* Flottant */Block.__(1, [Utils$ReactTemplate.super_float_of_string(chaine)]);
  } else if (chaine === "true" || chaine === "false") {
    return /* Booleen */Block.__(6, [Pervasives.bool_of_string(chaine)]);
  } else if (Caml_string.get(chaine, 0) === /* "\"" */34) {
    return /* Chaine */Block.__(3, [Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)]);
  } else if (Caml_string.get(chaine, 0) === /* "'" */39) {
    return /* Carac */Block.__(2, [Caml_string.get(chaine, 1)]);
  } else if (Caml_string.get(chaine, 0) === /* "(" */40) {
    var temp = List.map(to_token, third_cut(Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)));
    return /* Proce */Block.__(7, [temp]);
  } else if (Caml_string.get(chaine, 0) === /* "[" */91) {
    var temp$1 = List.map(to_token, third_cut(Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)));
    return /* TableauLex */Block.__(9, [temp$1]);
  } else if (Caml_string.get(chaine, 0) === /* "{" */123) {
    var temp$2 = List.map(to_token, third_cut(Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)));
    return /* Bloc */Block.__(8, [temp$2]);
  } else if (full_test((/:[^\s]+/), chaine)) {
    return /* Symbol */Block.__(4, [Utils$ReactTemplate.string_slice(1, chaine.length, chaine)]);
  } else if (full_test((/[^\s]+/), chaine)) {
    return /* Nom */Block.__(5, [chaine]);
  } else {
    return /* Unit */0;
  }
}

function look_untill(liste) {
  var _i = 1;
  while(true) {
    var i = _i;
    if (i === List.length(liste)) {
      return undefined;
    } else if (first_char_at(liste, i) === /* " " */32 || first_char_at(liste, i) === /* "\t" */9) {
      _i = i + 1 | 0;
      continue ;
    } else {
      return i;
    }
  };
}

function second_cut(liste) {
  var aux = function (_acc, _listb) {
    while(true) {
      var listb = _listb;
      var acc = _acc;
      var match = look_untill(listb);
      if (match !== undefined) {
        var i = match;
        _listb = Utils$ReactTemplate.list_slice(i, List.length(listb), listb);
        _acc = /* :: */[
          Utils$ReactTemplate.list_slice(0, i, listb),
          acc
        ];
        continue ;
      } else {
        return /* :: */[
                listb,
                acc
              ];
      }
    };
  };
  var result = List.rev(aux(/* [] */0, liste));
  return List.map((function (x) {
                return $$String.concat(" ", List.map($$String.trim, x));
              }), result);
}

function find_first_char(car, chaine) {
  var _i = 0;
  while(true) {
    var i = _i;
    var x = Caml_string.get(chaine, i);
    if (x === car) {
      return i;
    } else if (i === (chaine.length - 1 | 0)) {
      return undefined;
    } else {
      _i = i + 1 | 0;
      continue ;
    }
  };
}

function first_cut(car, chaine) {
  var match = find_first_char(car, chaine);
  if (match !== undefined) {
    var i = match;
    return /* :: */[
            $$String.sub(chaine, 0, i),
            first_cut(car, $$String.sub(chaine, i + 1 | 0, (chaine.length - i | 0) - 1 | 0))
          ];
  } else {
    return /* :: */[
            chaine,
            /* [] */0
          ];
  }
}

function interpete(chaine) {
  StdDef$ReactTemplate.sortie[0] = "";
  var code = List.map((function (x) {
          return List.map(to_token, third_cut(x));
        }), second_cut(List.filter((function (x) {
                    if ($$String.trim(x) !== "") {
                      return Caml_string.get($$String.trim(x), 0) !== /* "#" */35;
                    } else {
                      return false;
                    }
                  }))(first_cut(/* "\n" */10, chaine))));
  List.iter((function (x) {
          StdDef$ReactTemplate.run(x);
          return /* () */0;
        }), code);
  ImutHash$ReactTemplate.test(/* () */0);
  return StdDef$ReactTemplate.sortie[0];
}

var vars = StdDef$ReactTemplate.vars;

var get_value = StdDef$ReactTemplate.get_value;

var set_value = StdDef$ReactTemplate.set_value;

var add_stack = StdDef$ReactTemplate.add_stack;

var remove_stack = StdDef$ReactTemplate.remove_stack;

var get_stack = StdDef$ReactTemplate.get_stack;

var get_var = StdDef$ReactTemplate.get_var;

var sortie = StdDef$ReactTemplate.sortie;

var to_int = StdDef$ReactTemplate.to_int;

var to_float = StdDef$ReactTemplate.to_float;

var to_char = StdDef$ReactTemplate.to_char;

var to_string = StdDef$ReactTemplate.to_string;

var to_sym = StdDef$ReactTemplate.to_sym;

var to_bool = StdDef$ReactTemplate.to_bool;

var to_array = StdDef$ReactTemplate.to_array;

var to_block = StdDef$ReactTemplate.to_block;

var to_function = StdDef$ReactTemplate.to_function;

var run = StdDef$ReactTemplate.run;

var tok_get = StdDef$ReactTemplate.tok_get;

var tok_calc = StdDef$ReactTemplate.tok_calc;

var run_fun = StdDef$ReactTemplate.run_fun;

var look_at = StdDef$ReactTemplate.look_at;

exports.vars = vars;
exports.get_value = get_value;
exports.set_value = set_value;
exports.add_stack = add_stack;
exports.remove_stack = remove_stack;
exports.get_stack = get_stack;
exports.get_var = get_var;
exports.sortie = sortie;
exports.to_int = to_int;
exports.to_float = to_float;
exports.to_char = to_char;
exports.to_string = to_string;
exports.to_sym = to_sym;
exports.to_bool = to_bool;
exports.to_array = to_array;
exports.to_block = to_block;
exports.to_function = to_function;
exports.run = run;
exports.tok_get = tok_get;
exports.tok_calc = tok_calc;
exports.run_fun = run_fun;
exports.look_at = look_at;
exports.first_char_at = first_char_at;
exports.full_test = full_test;
exports.find_next_char = find_next_char;
exports.find_matching_char = find_matching_char;
exports.decrement_opt = decrement_opt;
exports.third_cut = third_cut;
exports.to_token = to_token;
exports.look_untill = look_untill;
exports.second_cut = second_cut;
exports.find_first_char = find_first_char;
exports.first_cut = first_cut;
exports.interpete = interpete;
/* StdDef-ReactTemplate Not a pure module */
