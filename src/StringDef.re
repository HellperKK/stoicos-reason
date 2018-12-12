include StdDef;

let string_mod:ImutHash.t(string, token) = ImutHash.empty
  |> ImutHash.add("concat", Fonction(NativeF(tokens => {
    let chaines = List.map(to_string, tokens);
    let chaine = String.concat("", chaines);
    Chaine(chaine);
  })))
  |> ImutHash.add("capitalize", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    Chaine(String.capitalize(chaine));
  })))
  |> ImutHash.add("uncapitalize", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    Chaine(String.uncapitalize(chaine));
  })))
  |> ImutHash.add("uppercase", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    Chaine(String.uppercase(chaine));
  })))
  |> ImutHash.add("lowercase", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    Chaine(String.lowercase(chaine));
  })))

  set_value("String", Struct(string_mod));
