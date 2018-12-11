include StringDef;

let first_char_at = (liste, i) => List.nth(liste, i).[0];

let full_test = (reg, chaine) => switch(Js.Re.exec(chaine, reg)){
  |None => false
  |Some(x) => {
    let capts = Js.Re.captures(x);
    switch(Js.Nullable.toOption(capts[0])){
      |None => false
      |Some(xb) => xb == chaine;
    };
  };
};

/* troisieme filtre */
let find_next_char = (car, chaine) => {
  let rec aux = i => switch(i){
    |x when x >= String.length(chaine)-1 => None
    |x when chaine.[x] == car => Some(x)
    |x => aux(x+1)
  };
  aux(1);
};

let find_matching_char = (car, carb, chaine) => {
  let rec aux = (i, compteur) => switch(i, compteur){
    |(x, _) when x >= String.length(chaine)-1 => None
    |(x, 0) => Some(x)
    |(x, c) when chaine.[x] == car => aux(x+1, c-1)
    |(x, c) when chaine.[x] == carb => aux(x+1, c+1)
    |(x, c) => aux(x+1, c)
  };
  aux(1, 1);
};

let decrement_opt = x => switch(x){
  |None => None
  |Some(x) => Some(x-1)
};

let rec third_cut = chaine => {
  let rec aux = (acc, chaineb) => {
    /* Js.log(chaineb); */
    let chaineb = String.trim(chaineb);
    switch(chaineb){
      |"" => acc
      |_ => {
        let milieu = switch(chaineb.[0]){
          |'\'' => find_next_char('\'', chaineb);
          |'"' => find_next_char('"', chaineb);
          |'(' => find_matching_char(')', '(', chaineb);
          |'[' => find_matching_char(']', '[', chaineb);
          |'{' => find_matching_char('}', '{', chaineb);
          |_ => find_next_char(' ', chaineb) |> decrement_opt;
        };
        switch(milieu){
          |None => [chaineb, ...acc]
          |Some(x) => {
            /* Js.log(x); */
            let debut = Utils.string_slice(0, x+1, chaineb) |> String.trim;
            let fin = Utils.string_slice(x+1, String.length(chaineb), chaineb) |> String.trim;
            /* Js.log(debut); */
            aux([debut, ...acc], fin);
          };
        };
      };
    };
  };
  aux([], chaine) |> List.rev
}

/* conversion en token */
and to_token = chaine => switch(chaine){
  |x when full_test([%bs.re "/[1-9]*[0-9]/"], x) => Entier(Utils.super_int_of_string(x));
  |x when full_test([%bs.re "/[1-9]*[0-9]\.[0-9]*/"], x) => Flottant(Utils.super_float_of_string(x));
  |x when (x == "true") || (x == "false") => Booleen(bool_of_string(x));
  |x when x.[0] == '"' => {
    Chaine(Utils.string_slice(1, String.length(x)-1, x));
  };
  |x when x.[0] == '\'' => Carac(x.[1]);
  |x when x.[0] == '(' => {
    let temp = Utils.string_slice(1, String.length(x)-1, x) |> third_cut |> List.map(to_token);
    Proce(temp);
  };
  |x when x.[0] == '[' => {
    let temp = Utils.string_slice(1, String.length(x)-1, x) |> third_cut |> List.map(to_token);
    TableauLex(temp);
  };
  |x when x.[0] == '{' => {
    let temp = Utils.string_slice(1, String.length(x)-1, x) |> third_cut |> List.map(to_token);
    Bloc(temp);
  };
  |x when full_test([%bs.re "/:[^\s]+/"], x) => {
    Symbol(Utils.string_slice(1, String.length(x), x))
  };
  |x when full_test([%bs.re "/[^\s]+\.[^\s]+/"], x) => {
    let noms = Utils.string_split('.', x);
    NSpace(List.nth(noms, 0), List.nth(noms, 1))
  };
  |x when full_test([%bs.re "/[^\s]+/"], x) => Nom(x);
  |_ => Unit;
};

/* second filtre */
let look_untill = liste => {
  let rec aux = i => switch(i){
    |x when x == List.length(liste) => None;
    |x when (first_char_at(liste, x) == ' ') || (first_char_at(liste, x) == '\t') => aux(x+1);
    |x => Some(x);
  };
  aux(1)
};

let second_cut = liste => {
  let rec aux = (acc, listb) => switch(look_untill(listb)){
    |None => [listb, ...acc]
    |Some(i) => aux([Utils.list_slice(0, i, listb), ...acc], Utils.list_slice(i, List.length(listb), listb))
  };
  let result = List.rev(aux([], liste));
  List.map((x) => List.map(String.trim, x) |> String.concat(" "), result);
};

let interpete = chaine => {
  sortie := ""
  if (chaine != "") {
    let code = Utils.string_split('\n', chaine)
      |> List.filter(x => (String.trim(x) != "") && (String.trim(x).[0] != '#'))
      |> second_cut
      |> List.map(x => x |> third_cut |> List.map(to_token));
    List.iter(x => x |> run |> ignore, code);
  }
  /* ImutHash.test(); */
  sortie^;
};
