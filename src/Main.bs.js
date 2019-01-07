// Generated by BUCKLESCRIPT VERSION 4.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Hashtbl = require("bs-platform/lib/js/hashtbl.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Utils$ReactTemplate = require("./Utils.bs.js");
var ImutHash$ReactTemplate = require("./ImutHash.bs.js");

var vars = /* record */[/* contents : :: */[
    Hashtbl.create(undefined, 100),
    /* [] */0
  ]];

var sortie = /* record */[/* contents */""];

function get_value(name) {
  return List.fold_left((function (memo, value) {
                var match = memo === /* Unit */0 && Hashtbl.mem(value, name);
                if (match) {
                  return Hashtbl.find(value, name);
                } else {
                  return memo;
                }
              }), /* Unit */0, vars[0]);
}

function set_value(name, value) {
  var match = vars[0];
  if (match) {
    return Hashtbl.replace(match[0], name, value);
  } else {
    return /* () */0;
  }
}

function add_stack() {
  var ancien = vars[0];
  vars[0] = /* :: */[
    Hashtbl.create(undefined, 100),
    ancien
  ];
  return /* () */0;
}

function remove_stack() {
  var match = vars[0];
  var tmp;
  if (match) {
    var tail = match[1];
    tmp = tail ? tail : /* :: */[
        match[0],
        /* [] */0
      ];
  } else {
    tmp = /* [] */0;
  }
  vars[0] = tmp;
  return /* () */0;
}

function get_stack() {
  var match = vars[0];
  if (match) {
    return match[0];
  } else {
    return Hashtbl.create(undefined, 100);
  }
}

function get_var(tok) {
  if (typeof tok === "number") {
    return tok;
  } else {
    switch (tok.tag | 0) {
      case 5 : 
          return get_value(tok[0]);
      case 6 : 
          var value = get_value(tok[0]);
          var hash;
          hash = typeof value === "number" || value.tag !== 13 ? ImutHash$ReactTemplate.empty : value[0];
          var match = ImutHash$ReactTemplate.find_opt(tok[1], hash);
          if (match !== undefined) {
            return match;
          } else {
            return /* Unit */0;
          }
      default:
        return tok;
    }
  }
}

function to_int(tok) {
  if (typeof tok === "number") {
    return 0;
  } else {
    switch (tok.tag | 0) {
      case 1 : 
          return tok[0] | 0;
      case 0 : 
      case 2 : 
          return tok[0];
      case 3 : 
      case 4 : 
          return Utils$ReactTemplate.super_int_of_string(tok[0]);
      default:
        return 0;
    }
  }
}

function to_float(tok) {
  if (typeof tok === "number") {
    return 0.0;
  } else {
    switch (tok.tag | 0) {
      case 1 : 
          return tok[0];
      case 0 : 
      case 2 : 
          return tok[0];
      case 3 : 
      case 4 : 
          return Utils$ReactTemplate.super_float_of_string(tok[0]);
      default:
        return 0.0;
    }
  }
}

function to_char(tok) {
  if (typeof tok === "number") {
    return /* " " */32;
  } else {
    switch (tok.tag | 0) {
      case 0 : 
          return Pervasives.char_of_int(tok[0]);
      case 1 : 
          return Pervasives.char_of_int(tok[0] | 0);
      case 2 : 
          return tok[0];
      case 3 : 
      case 4 : 
          return Caml_string.get(tok[0], 0);
      default:
        return /* " " */32;
    }
  }
}

function to_string(tok) {
  if (typeof tok === "number") {
    return "";
  } else {
    switch (tok.tag | 0) {
      case 0 : 
          return String(tok[0]);
      case 1 : 
          return Pervasives.string_of_float(tok[0]);
      case 2 : 
          return $$String.make(1, tok[0]);
      case 3 : 
      case 4 : 
          return tok[0];
      case 7 : 
          return Pervasives.string_of_bool(tok[0]);
      default:
        return "";
    }
  }
}

function to_sym(tok) {
  if (typeof tok === "number") {
    return "";
  } else {
    switch (tok.tag | 0) {
      case 0 : 
          return String(tok[0]);
      case 1 : 
          return Pervasives.string_of_float(tok[0]);
      case 2 : 
          return $$String.make(1, tok[0]);
      case 3 : 
      case 4 : 
          return tok[0];
      case 7 : 
          return Pervasives.string_of_bool(tok[0]);
      default:
        return "";
    }
  }
}

function to_bool(tok) {
  if (typeof tok === "number") {
    return false;
  } else if (tok.tag === 7) {
    return tok[0];
  } else {
    return true;
  }
}

function to_array(tok) {
  if (typeof tok === "number" || tok.tag !== 11) {
    return /* :: */[
            tok,
            /* [] */0
          ];
  } else {
    return tok[0];
  }
}

function to_block(tok) {
  if (typeof tok === "number" || tok.tag !== 9) {
    return /* :: */[
            tok,
            /* [] */0
          ];
  } else {
    return tok[0];
  }
}

function to_function(tok) {
  var exit = 0;
  if (typeof tok === "number" || tok.tag !== 12) {
    exit = 1;
  } else {
    return tok[0];
  }
  if (exit === 1) {
    return /* NativeF */Block.__(0, [(function () {
                  return /* Unit */0;
                })]);
  }
  
}

function run(tokens) {
  if (tokens) {
    var funcb = to_function(get_var(tokens[0]));
    var argsb = List.map((function (x) {
            return get_var(tok_get(x));
          }), tokens[1]);
    return run_fun(funcb, argsb);
  } else {
    return /* Unit */0;
  }
}

function tok_get(token) {
  if (typeof token === "number") {
    return token;
  } else {
    switch (token.tag | 0) {
      case 8 : 
          return run(token[0]);
      case 10 : 
          return /* Tableau */Block.__(11, [List.map((function (tok) {
                            return get_var(tok_get(tok));
                          }), token[0])]);
      default:
        return token;
    }
  }
}

function tok_calc(tokens) {
  return List.fold_left((function (_, tok) {
                return tok_get(tok);
              }), /* Unit */0, tokens);
}

function run_fun(func, args) {
  if (func.tag) {
    var names = func[0];
    add_stack(/* () */0);
    var argsb = Utils$ReactTemplate.list_same_size(names, args, /* Unit */0);
    List.iter2(set_value, names, argsb);
    var result = tok_calc(func[1]);
    remove_stack(/* () */0);
    return result;
  } else {
    return Curry._1(func[0], args);
  }
}

function look_at(liste, index) {
  return Utils$ReactTemplate.list_fetch(liste, index, /* Unit */0);
}

set_value("print", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var chaines = List.map(to_string, tokens);
                var chaine = $$String.concat(" ", chaines);
                sortie[0] = sortie[0] + chaine;
                return /* Chaine */Block.__(3, [chaine]);
              })])]));

set_value("=", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var name = to_sym(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                var value = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                set_value(name, value);
                return /* Unit */0;
              })])]));

set_value("assign", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var noms = List.map(to_sym, to_array(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0)));
                var value = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                List.iter((function (x) {
                        return set_value(x, value);
                      }), noms);
                return /* Unit */0;
              })])]));

set_value("bind", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var noms = List.map(to_sym, to_array(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0)));
                var value = to_array(Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0));
                var valueb = Utils$ReactTemplate.list_same_size(noms, value, /* Unit */0);
                List.iter2(set_value, noms, valueb);
                return /* Unit */0;
              })])]));

set_value("==", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var value = Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0);
                var valueb = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                return /* Booleen */Block.__(7, [Caml_obj.caml_equal(value, valueb)]);
              })])]));

set_value("!=", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var value = Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0);
                var valueb = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                return /* Booleen */Block.__(7, [Caml_obj.caml_notequal(value, valueb)]);
              })])]));

set_value(">", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var value = Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0);
                var valueb = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                return /* Booleen */Block.__(7, [Caml_obj.caml_greaterthan(value, valueb)]);
              })])]));

set_value(">=", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var value = Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0);
                var valueb = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                return /* Booleen */Block.__(7, [Caml_obj.caml_greaterequal(value, valueb)]);
              })])]));

set_value("<", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var value = Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0);
                var valueb = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                return /* Booleen */Block.__(7, [Caml_obj.caml_lessthan(value, valueb)]);
              })])]));

set_value("<=", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var value = Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0);
                var valueb = Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0);
                return /* Booleen */Block.__(7, [Caml_obj.caml_lessequal(value, valueb)]);
              })])]));

set_value("if", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var bool = to_bool(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                var bloc = to_block(Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0));
                var blocb = to_block(Utils$ReactTemplate.list_fetch(tokens, 2, /* Unit */0));
                if (bool) {
                  return tok_calc(bloc);
                } else {
                  return tok_calc(blocb);
                }
              })])]));

set_value("cond", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var conds = List.map(to_array, tokens);
                var elem = Utils$ReactTemplate.list_first((function (x) {
                        return to_bool(Utils$ReactTemplate.list_fetch(x, 0, /* Unit */0));
                      }), conds);
                if (elem !== undefined) {
                  return Utils$ReactTemplate.list_fetch(elem, 1, /* Unit */0);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("fun", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var args = List.map(to_sym, to_array(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0)));
                var bloc = to_block(Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0));
                return /* Fonction */Block.__(12, [/* CustomF */Block.__(1, [
                              args,
                              bloc
                            ])]);
              })])]));

set_value("not", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var bool = to_bool(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                return /* Booleen */Block.__(7, [!bool]);
              })])]));

set_value("or", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          if (prim) {
                            return true;
                          } else {
                            return prim$1;
                          }
                        }), to_bool(tokens[0]), List.map(to_bool, tokens[1]));
                  return /* Booleen */Block.__(7, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("and", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          if (prim) {
                            return prim$1;
                          } else {
                            return false;
                          }
                        }), to_bool(tokens[0]), List.map(to_bool, tokens[1]));
                  return /* Booleen */Block.__(7, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("^", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var chaines = List.map(to_string, tokens);
                var chaine = $$String.concat("", chaines);
                return /* Chaine */Block.__(3, [chaine]);
              })])]));

set_value("+", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left((function (prim, prim$1) {
                        return prim + prim$1 | 0;
                      }), 0, List.map(to_int, tokens));
                return /* Entier */Block.__(0, [result]);
              })])]));

set_value("*", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left(Caml_int32.imul, 1, List.map(to_int, tokens));
                return /* Entier */Block.__(0, [result]);
              })])]));

set_value("-", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim - prim$1 | 0;
                        }), to_int(tokens[0]), List.map(to_int, tokens[1]));
                  return /* Entier */Block.__(0, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("/", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left(Caml_int32.div, to_int(tokens[0]), List.map(to_int, tokens[1]));
                  return /* Entier */Block.__(0, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("%", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left(Caml_int32.mod_, to_int(tokens[0]), List.map(to_int, tokens[1]));
                  return /* Entier */Block.__(0, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("+.", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left((function (prim, prim$1) {
                        return prim + prim$1;
                      }), 0, List.map(to_float, tokens));
                return /* Flottant */Block.__(1, [result]);
              })])]));

set_value("*.", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var result = List.fold_left((function (prim, prim$1) {
                        return prim * prim$1;
                      }), 1.0, List.map(to_float, tokens));
                return /* Flottant */Block.__(1, [result]);
              })])]));

set_value("-.", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim - prim$1;
                        }), to_float(tokens[0]), List.map(to_float, tokens[1]));
                  return /* Flottant */Block.__(1, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("/.", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim / prim$1;
                        }), to_float(tokens[0]), List.map(to_float, tokens[1]));
                  return /* Flottant */Block.__(1, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

set_value("%.", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                if (tokens) {
                  var result = List.fold_left((function (prim, prim$1) {
                          return prim % prim$1;
                        }), to_float(tokens[0]), List.map(to_float, tokens[1]));
                  return /* Flottant */Block.__(1, [result]);
                } else {
                  return /* Unit */0;
                }
              })])]));

var string_mod = ImutHash$ReactTemplate.add("length", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                return /* Entier */Block.__(0, [chaine.length]);
              })])]), ImutHash$ReactTemplate.add("get", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                    var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                    var i = to_int(Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0));
                    return /* Carac */Block.__(2, [Caml_string.get(chaine, i)]);
                  })])]), ImutHash$ReactTemplate.add("sub", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                        var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                        var deb = to_int(Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0));
                        var len = to_int(Utils$ReactTemplate.list_fetch(tokens, 2, /* Unit */0));
                        return /* Chaine */Block.__(3, [$$String.sub(chaine, deb, len)]);
                      })])]), ImutHash$ReactTemplate.add("slice", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                            var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                            var deb = to_int(Utils$ReactTemplate.list_fetch(tokens, 1, /* Unit */0));
                            var fin = to_int(Utils$ReactTemplate.list_fetch(tokens, 2, /* Unit */0));
                            return /* Chaine */Block.__(3, [Utils$ReactTemplate.string_slice(deb, fin, chaine)]);
                          })])]), ImutHash$ReactTemplate.add("trim", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                                var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                                return /* Chaine */Block.__(3, [$$String.trim(chaine)]);
                              })])]), ImutHash$ReactTemplate.add("lowercase", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                                    var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                                    return /* Chaine */Block.__(3, [$$String.lowercase(chaine)]);
                                  })])]), ImutHash$ReactTemplate.add("uppercase", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                                        var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                                        return /* Chaine */Block.__(3, [$$String.uppercase(chaine)]);
                                      })])]), ImutHash$ReactTemplate.add("uncapitalize", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                                            var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                                            return /* Chaine */Block.__(3, [$$String.uncapitalize(chaine)]);
                                          })])]), ImutHash$ReactTemplate.add("capitalize", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                                                var chaine = to_string(Utils$ReactTemplate.list_fetch(tokens, 0, /* Unit */0));
                                                return /* Chaine */Block.__(3, [$$String.capitalize(chaine)]);
                                              })])]), ImutHash$ReactTemplate.add("concat", /* Fonction */Block.__(12, [/* NativeF */Block.__(0, [(function (tokens) {
                                                    var chaines = List.map(to_string, tokens);
                                                    var chaine = $$String.concat("", chaines);
                                                    return /* Chaine */Block.__(3, [chaine]);
                                                  })])]), ImutHash$ReactTemplate.empty))))))))));

set_value("String", /* Struct */Block.__(13, [string_mod]));

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
  } else if (full_test((/[1-9]*[0-9]\.[0-9]*/), chaine)) {
    return /* Flottant */Block.__(1, [Utils$ReactTemplate.super_float_of_string(chaine)]);
  } else if (chaine === "true" || chaine === "false") {
    return /* Booleen */Block.__(7, [Pervasives.bool_of_string(chaine)]);
  } else if (Caml_string.get(chaine, 0) === /* "\"" */34) {
    return /* Chaine */Block.__(3, [Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)]);
  } else if (Caml_string.get(chaine, 0) === /* "'" */39) {
    return /* Carac */Block.__(2, [Caml_string.get(chaine, 1)]);
  } else if (Caml_string.get(chaine, 0) === /* "(" */40) {
    var temp = List.map(to_token, third_cut(Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)));
    return /* Proce */Block.__(8, [temp]);
  } else if (Caml_string.get(chaine, 0) === /* "[" */91) {
    var temp$1 = List.map(to_token, third_cut(Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)));
    return /* TableauLex */Block.__(10, [temp$1]);
  } else if (Caml_string.get(chaine, 0) === /* "{" */123) {
    var temp$2 = List.map(to_token, third_cut(Utils$ReactTemplate.string_slice(1, chaine.length - 1 | 0, chaine)));
    return /* Bloc */Block.__(9, [temp$2]);
  } else if (full_test((/:[^\s]+/), chaine)) {
    return /* Symbol */Block.__(4, [Utils$ReactTemplate.string_slice(1, chaine.length, chaine)]);
  } else if (full_test((/[^\s]+\.[^\s]+/), chaine)) {
    var noms = Utils$ReactTemplate.string_split(/* "." */46, chaine);
    return /* NSpace */Block.__(6, [
              List.nth(noms, 0),
              List.nth(noms, 1)
            ]);
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

function interpete(chaine) {
  sortie[0] = "";
  if (chaine !== "") {
    var code = List.map((function (x) {
            return List.map(to_token, third_cut(x));
          }), second_cut(List.filter((function (x) {
                      if ($$String.trim(x) !== "") {
                        return Caml_string.get($$String.trim(x), 0) !== /* "#" */35;
                      } else {
                        return false;
                      }
                    }))(Utils$ReactTemplate.string_split(/* "\n" */10, chaine))));
    List.iter((function (x) {
            run(x);
            return /* () */0;
          }), code);
  }
  return sortie[0];
}

exports.vars = vars;
exports.sortie = sortie;
exports.get_value = get_value;
exports.set_value = set_value;
exports.add_stack = add_stack;
exports.remove_stack = remove_stack;
exports.get_stack = get_stack;
exports.get_var = get_var;
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
exports.string_mod = string_mod;
exports.first_char_at = first_char_at;
exports.full_test = full_test;
exports.find_next_char = find_next_char;
exports.find_matching_char = find_matching_char;
exports.decrement_opt = decrement_opt;
exports.third_cut = third_cut;
exports.to_token = to_token;
exports.look_untill = look_untill;
exports.second_cut = second_cut;
exports.interpete = interpete;
/* vars Not a pure module */
