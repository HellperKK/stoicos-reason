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
  |Proce(list(token))
  |Bloc(list(token))
  |TableauLex(list(token))
  |Tableau(list(token))
  |Fonction(fonction)
  |Struct(Hashtbl.t(string, token))
  |Unit

and fonction =
  |NativeF(list(token)=>token)
  |CustomF(list(string), list(token));

let vars:ref(list(Hashtbl.t(string, token))) = ref([Hashtbl.create(100)]);

let get_value = name => {
  List.fold_left((memo, value) => {
    Hashtbl.mem(value, name) && memo == Unit ? Hashtbl.find(value, name) : memo
  }, Unit, vars^)
};

let add_stack = () => {
  vars := {
    let ancien = vars^;
    [Hashtbl.create(100), ...ancien];
  };
};

let remove_stack = () => {
  vars := switch(vars^){
    |[] => []
    |[head] => [head]
    |[_, ...tail] => tail
  };
};

let get_stack = () => switch(vars^){
  |[] => Hashtbl.create(100)
  |[head, ..._] => head
};

let set_value = (name, value) => switch(vars^){
  |[] => ();
  |[head, ..._] => Hashtbl.replace(head, name, value)
};

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
  |Booleen(x) => string_of_bool(x);
  |_ => ""
};

let to_bool = tok => switch(tok){
  |Booleen(x) => x;
  |Unit => false;
  |_ => true
};

let to_function = tok => switch(tok){
  |Fonction(x) => x;
  |_ => NativeF(_ => Unit)
};

let get_var = tok => switch(tok){
  |Nom(x) => get_value(x)
  |x => x
};

/* lancement d'une fonction */
let rec run = tokens => switch(tokens){
  |[] => Unit
  |[func, ...args] => {
    let funcb = get_var(func) |> to_function
    let argsb = List.map(x => x |> tok_get |> get_var, args)
    run_fun(funcb, argsb)
  };
}

and tok_get = token => switch(token){
  |Proce(tokens) => run(tokens);
  |TableauLex(tokens) => Tableau(List.map(tok => tok |> tok_get |> get_var, tokens))
  |x => x;
}

and tok_calc = token => switch(token){
  |Proce(tokens) => run(tokens);
  |Bloc(tokens) => {
    List.fold_left((_, tok) => tok_get(tok), Unit, tokens)
  };
  |x => x;
}

and run_fun = (func, args) => switch(func){
  |NativeF(x) => x(args)
  |_ => Unit
};
