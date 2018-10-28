include Type;

let cust_print = tokens => {
  let chaines = List.map(to_string, tokens);
  let chaine = String.concat(" ", chaines);
  /* Js.log(chaine); */
  sortie := sortie^ ++ chaine
  Chaine(chaine);
};

let cust_add = tokens => {
  let result = List.map(to_int, tokens)
    |> List.fold_left((+), 0);
  Entier(result);
};

let cust_times = tokens => {
  let result = List.map(to_int, tokens)
    |> List.fold_left((*), 1);
  Entier(result);
};

let cust_rem = tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_int, tail)
      |> List.fold_left((-), to_int(head));
    Entier(result);
  };
};

let cust_div = tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_int, tail)
      |> List.fold_left((/), to_int(head));
    Entier(result);
  };
};

let cust_mod = tokens => switch(tokens){
  |[] => Unit
  |[head, ...tail] => {
    let result = List.map(to_int, tail)
      |> List.fold_left((mod), to_int(head));
    Entier(result);
  };
};

let _ = {
  Hashtbl.add(vars, "print", NativeFonction(cust_print));
  Hashtbl.add(vars, "+", NativeFonction(cust_add));
  Hashtbl.add(vars, "*", NativeFonction(cust_times));
  Hashtbl.add(vars, "-", NativeFonction(cust_rem));
  Hashtbl.add(vars, "/", NativeFonction(cust_div));
  Hashtbl.add(vars, "%", NativeFonction(cust_mod));
};
