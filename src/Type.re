/* Int -> present
Float -> present
String -> present
Synbole -> present
Variable -> present
Boolean -> present
Proc -> present
Bloc -> present
Array -> present
Unit -> present
Char
Fonction -> present
Struct -> present */

type token =
  |Entier(int)
  |Flottant(float)
  |Carac(char)
  |Chaine(string)
  |Symbol(string)
  |Nom(string)
  |Booleen(bool)
  |Proc(list(token))
  |Block(list(token))
  |Array(list(token))
  |NativeFonction(list(token)=>token)
  |CustomFonction(list(string), list(token))
  |Struct(Hashtbl.t(string, token))
  |Unit

let vars:Hashtbl.t(string, token) = Hashtbl.create(100);
let sortie = ref("");

let to_int = tok => switch(tok){
  |Entier(x) => x;
  |Flottant(x) => int_of_float(x);
  |Carac(x) => int_of_char(x);
  |Chaine(x) => Utils.super_int_of_string(x);
  |Symbol(x) => Utils.super_int_of_string(x);
  |_ => 0
};

let to_float = tok => switch(tok){
  |Entier(x) => float_of_int(x);
  |Flottant(x) => x;
  |Carac(x) => float_of_int(int_of_char(x));
  |Chaine(x) => Utils.super_float_of_string(x);
  |Symbol(x) => Utils.super_float_of_string(x);
  |_ => 0.0
};

let to_char = tok => switch(tok){
  |Entier(x) => char_of_int(x);
  |Flottant(x) => char_of_int(int_of_float(x));
  |Carac(x) => x;
  |Chaine(x) => x.[0];
  |Symbol(x) => x.[0];
  |_ => ' '
};

let to_string = tok => switch(tok){
  |Entier(x) => string_of_int(x);
  |Flottant(x) => string_of_float(x);
  |Carac(x) => String.make(1, x);
  |Chaine(x) => x;
  |Symbol(x) => x;
  |_ => ""
};

let to_function = tok => switch(tok){
  |NativeFonction(x) => NativeFonction(x);
  |_ => NativeFonction(_ => Unit)
};

let get_var = tok => switch(tok){
  |Nom(x) => Hashtbl.find(vars, x)
  |x => x
};

let run_fun = (func, args) => switch(func){
  |NativeFonction(x) => x(args)
  |_ => Unit
};
