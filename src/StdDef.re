include Type;

/* fonctions IO */
set_value("print", Fonction(NativeF(tokens => {
  let chaines = List.map(to_string, tokens);
  let chaine = String.concat(" ", chaines);
  sortie := sortie^ ++ chaine
  Chaine(chaine);
})));

/* fonction variables */
set_value("=", Fonction(NativeF(tokens => {
  let name = List.nth(tokens, 0) |> to_sym;
  let value = List.nth(tokens, 1) |> tok_get |> get_var;
  set_value(name, value);
  Unit
})));

/* fonctions booleens */
set_value("not", Fonction(NativeF(tokens => {
  let bool = List.nth(tokens, 0) |> to_bool;
  Booleen(! bool);
})));

set_value("or", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_bool, tail)
      |> List.fold_left((||), to_bool(head));
    Booleen(result);
  };
})));

set_value("and", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_bool, tail)
      |> List.fold_left((&&), to_bool(head));
    Booleen(result);
  };
})));

/* fonctions chaines */
set_value("^", Fonction(NativeF(tokens => {
  let chaines = List.map(to_string, tokens);
  let chaine = String.concat("", chaines);
  Chaine(chaine);
})));

/* fonctions entiers */
set_value("+", Fonction(NativeF(tokens => {
  let result = List.map(to_int, tokens)
    |> List.fold_left((+), 0);
  Entier(result);
})));
set_value("*", Fonction(NativeF(tokens => {
  let result = List.map(to_int, tokens)
    |> List.fold_left((*), 1);
  Entier(result);
})));
set_value("-", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_int, tail)
      |> List.fold_left((-), to_int(head));
    Entier(result);
  };
})));
set_value("/", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_int, tail)
      |> List.fold_left((/), to_int(head));
    Entier(result);
  };
})));
set_value("%", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_int, tail)
      |> List.fold_left((mod), to_int(head));
    Entier(result);
  };
})));

/* fonctions flottants */
set_value("+.", Fonction(NativeF(tokens => {
  let result = List.map(to_float, tokens)
    |> List.fold_left((+.), 0.);
  Flottant(result);
})));
set_value("*.", Fonction(NativeF(tokens => {
  let result = List.map(to_float, tokens)
    |> List.fold_left((*.), 1.0);
  Flottant(result);
})));
set_value("-.", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_float, tail)
      |> List.fold_left((-.), to_float(head));
    Flottant(result);
  };
})));
set_value("/.", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_float, tail)
      |> List.fold_left((/.), to_float(head));
    Flottant(result);
  };
})));
set_value("%.", Fonction(NativeF(tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_float, tail)
      |> List.fold_left((mod_float), to_float(head));
    Flottant(result);
  };
})));
