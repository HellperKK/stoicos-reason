include StdDef;

let string_mod:ImutHash.t(string, token) = ImutHash.empty
  |> ImutHash.add("concat", Fonction(NativeF(tokens => {
    let chaines = List.map(to_string, tokens);
    let chaine = String.concat("", chaines);
    Chaine(chaine);
  })))

  set_value("String", Struct(string_mod));
