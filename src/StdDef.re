include Type;

let look_at = (liste, index) => Utils.list_fetch(liste, index, Unit)

/* fonctions IO */
set_value("print", Fonction(NativeF(tokens => {
  let chaines = List.map(to_string, tokens);
  let chaine = String.concat(" ", chaines);
  sortie := sortie^ ++ chaine
  Chaine(chaine);
})));

/* fonction variables */
set_value("=", Fonction(NativeF(tokens => {
  let name = look_at(tokens, 0) |> to_sym;
  let value = look_at(tokens, 1);
  set_value(name, value);
  Unit
})));

set_value("assign", Fonction(NativeF(tokens => {
  let noms = look_at(tokens, 0) |> to_array |> List.map(to_sym);
  let value = look_at(tokens, 1)
  List.iter(x => set_value(x, value), noms)
  Unit
})));

set_value("bind", Fonction(NativeF(tokens => {
  let noms = look_at(tokens, 0) |> to_array |> List.map(to_sym);
  let value = look_at(tokens, 1)|> to_array
  let valueb = Utils.list_same_size(noms, value, Unit)
  List.iter2((name, valeur) => set_value(name, valeur), noms, valueb)
  Unit
})));

/* fonctions blocs */
set_value("if", Fonction(NativeF(tokens => {
  let bool = look_at(tokens, 0) |> to_bool;
  let bloc = look_at(tokens, 1) |> to_block;
  let blocb = look_at(tokens, 2) |> to_block;
  bool ? tok_calc(bloc) : tok_calc(blocb)
})));

/* fonction construction */
set_value("fun", Fonction(NativeF(tokens => {
  let args = look_at(tokens, 0) |> to_array |> List.map(to_sym);
  let bloc = look_at(tokens, 1) |> to_block;
  Fonction(CustomF(args, bloc))
})));

/* fonctions booleens */
set_value("not", Fonction(NativeF(tokens => {
  let bool = look_at(tokens, 0) |> to_bool;
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
