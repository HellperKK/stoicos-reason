/* part dedicated to the type system */

/* reprensent tokens type for each type of value in the system */
type token =
  |Entier(int)
  |Flottant(float)
  |Carac(char)
  |Chaine(string)
  |Symbol(string)
  |Nom(string)
  |NSpace(string, string)
  |Booleen(bool)
  |Proce(list(token))
  |Bloc(list(token))
  |TableauLex(list(token))
  |Tableau(list(token))
  |Fonction(fonction)
  |Struct(ImutHash.t(string, token))
  |Unit
  |Undef

/* represent subtokens for functions */
and fonction =
  |NativeF(list(token)=>token)
  |CustomF(list(string), list(token));

/* variable that holds every variiables of the language */
let vars:ref(list(Hashtbl.t(string, token))) = ref([Hashtbl.create(100)]);

/* variable that holds the standard output */
let sortie = ref("");

/* get a variable value */
let get_value = name => {
  let value = List.fold_left((memo, value) => {
    memo == Undef && Hashtbl.mem(value, name) ? Hashtbl.find(value, name) : memo
  }, Undef, vars^);
  value == Undef ? Unit : value;
};

/* set a variable value */
let set_value = (name, value) => switch(vars^){
  |[] => ();
  |[head, ..._] => Hashtbl.replace(head, name, value)
};

/* adds a stack for custom function call */
let add_stack = () => {
  vars := {
    let ancien = vars^;
    [Hashtbl.create(100), ...ancien];
  };
};

/* removes a stack */
let remove_stack = () => {
  vars := switch(vars^){
    |[] => []
    |[head] => [head]
    |[_, ...tail] => tail
  };
};

/* get the most recent stack */
let get_stack = () => switch(vars^){
  |[] => Hashtbl.create(100)
  |[head, ..._] => head
};

/* get a variable from a token */
let get_var = tok => switch(tok){
  |Nom(x) => get_value(x)
  |NSpace(x, sub) => {
    let value = get_value(x);
    let hash = switch(value){
      |Struct(x) => x;
      |_ => ImutHash.empty;
    }
    switch(ImutHash.find_opt(sub, hash)){
      |None => Unit;
      |Some(x) => x;
    }
  }
  |x => x
};

/* get an int form a token */
let to_int = tok => switch(tok){
  |Entier(x) => x;
  |Flottant(x) => int_of_float(x);
  |Carac(x) => int_of_char(x);
  |Chaine(x) => Utils.super_int_of_string(x);
  |Symbol(x) => Utils.super_int_of_string(x);
  |_ => 0
};

/* get a float form a token */
let to_float = tok => switch(tok){
  |Entier(x) => float_of_int(x);
  |Flottant(x) => x;
  |Carac(x) => float_of_int(int_of_char(x));
  |Chaine(x) => Utils.super_float_of_string(x);
  |Symbol(x) => Utils.super_float_of_string(x);
  |_ => 0.0
};

/* get a caracter form a token */
let to_char = tok => switch(tok){
  |Entier(x) => char_of_int(x);
  |Flottant(x) => char_of_int(int_of_float(x));
  |Carac(x) => x;
  |Chaine(x) => x.[0];
  |Symbol(x) => x.[0];
  |_ => ' '
};

/* get a string form a token */
let to_string = tok => switch(tok){
  |Entier(x) => string_of_int(x);
  |Flottant(x) => string_of_float(x);
  |Carac(x) => String.make(1, x);
  |Chaine(x) => x;
  |Symbol(x) => x;
  |Booleen(x) => string_of_bool(x);
  |_ => ""
};

/* get a symbol form a token */
let to_sym = tok => switch(tok){
  |Entier(x) => string_of_int(x);
  |Flottant(x) => string_of_float(x);
  |Carac(x) => String.make(1, x);
  |Chaine(x) => x;
  |Symbol(x) => x;
  |Booleen(x) => string_of_bool(x);
  |_ => ""
};

/* get a boolean form a token */
let to_bool = tok => switch(tok){
  |Booleen(x) => x;
  |Unit => false;
  |_ => true;
};

/* get a list form a token */
let to_array = tok => switch(tok){
  |Tableau(x) => x;
  |x => [x];
};

/* get a block form a token */
let to_block = tok => switch(tok){
  |Bloc(x) => x;
  |x => [x];
};

/* get a function form a token */
let to_function = tok => switch(tok){
  |Fonction(x) => x;
  |_ => NativeF(_ => Unit)
};

/* run globally a function */
let rec run = tokens => switch(tokens){
  |[] => Unit
  |[func, ...args] => {
    let funcb = get_var(func) |> to_function
    let argsb = List.map(x => x |> tok_get |> get_var, args)
    run_fun(funcb, argsb)
  };
}

/* gets a value from a procedure or an intermediary list */
and tok_get = token => switch(token){
  |Proce(tokens) => run(tokens);
  |TableauLex(tokens) => Tableau(List.map(tok => tok |> tok_get |> get_var, tokens))
  |x => x;
}

/* calculates a block */
and tok_calc = tokens => {
  List.fold_left((_, tok) => tok_get(tok), Unit, tokens)
}

/* runs a function */
and run_fun = (func, args) => switch(func){
  |NativeF(x) => x(args)
  |CustomF(names, code) => {
    add_stack();
    let argsb = Utils.list_same_size(names, args, Unit);
    List.iter2((name, value) => set_value(name, value), names, argsb)
    let result = tok_calc(code);
    remove_stack();
    result;
  }
};



/* part dedicated to the base functions */
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
  let value = look_at(tokens, 1);
  List.iter(x => set_value(x, value), noms);
  Unit;
})));

set_value("bind", Fonction(NativeF(tokens => {
  let noms = look_at(tokens, 0) |> to_array |> List.map(to_sym);
  let value = look_at(tokens, 1)|> to_array;
  let valueb = Utils.list_same_size(noms, value, Unit);
  List.iter2((name, valeur) => set_value(name, valeur), noms, valueb);
  Unit;
})));

/* fonctions comparaison */
set_value("==", Fonction(NativeF(tokens => {
  let value = look_at(tokens, 0);
  let valueb = look_at(tokens, 1);
  Booleen(value == valueb);
})));

set_value("!=", Fonction(NativeF(tokens => {
  let value = look_at(tokens, 0);
  let valueb = look_at(tokens, 1);
  Booleen(value != valueb);
})));

set_value(">", Fonction(NativeF(tokens => {
  let value = look_at(tokens, 0);
  let valueb = look_at(tokens, 1);
  Booleen(value > valueb);
})));

set_value(">=", Fonction(NativeF(tokens => {
  let value = look_at(tokens, 0);
  let valueb = look_at(tokens, 1);
  Booleen(value >= valueb);
})));

set_value("<", Fonction(NativeF(tokens => {
  let value = look_at(tokens, 0);
  let valueb = look_at(tokens, 1);
  Booleen(value < valueb);
})));

set_value("<=", Fonction(NativeF(tokens => {
  let value = look_at(tokens, 0);
  let valueb = look_at(tokens, 1);
  Booleen(value <= valueb);
})));


/* fonctions blocs */
set_value("if", Fonction(NativeF(tokens => {
  let bool = look_at(tokens, 0) |> to_bool;
  let bloc = look_at(tokens, 1) |> to_block;
  let blocb = look_at(tokens, 2) |> to_block;
  bool ? tok_calc(bloc) : tok_calc(blocb)
})));

set_value("cond", Fonction(NativeF(tokens => {
  let conds = List.map(to_array, tokens);
  let elem = Utils.list_first(x => look_at(x, 0) |> to_bool, conds);
  switch(elem){
    |None => Unit
    |Some(x) => look_at(x, 1);
  };
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



/* part dedicated to the string module definition */
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
  |> ImutHash.add("trim", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    Chaine(String.trim(chaine));
  })))
  |> ImutHash.add("slice", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    let deb = look_at(tokens, 1) |> to_int;
    let fin = look_at(tokens, 2) |> to_int;
    Chaine(Utils.string_slice(deb, fin, chaine));
  })))
  |> ImutHash.add("sub", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    let deb = look_at(tokens, 1) |> to_int;
    let len = look_at(tokens, 2) |> to_int;
    Chaine(String.sub(chaine, deb, len));
  })))
  |> ImutHash.add("get", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    let i = look_at(tokens, 1) |> to_int;
    Carac(chaine.[i]);
  })))
  |> ImutHash.add("length", Fonction(NativeF(tokens => {
    let chaine = look_at(tokens, 0) |> to_string;
    Entier(String.length(chaine));
  })));
set_value("String", Struct(string_mod));

/* part dedicated to file lexing and global execution
of the code */

/* gives the firts caracter of the first element of a list(strings) */
let first_char_at = (liste, i) => List.nth(liste, i).[0];

/* checks if a string is fully matched by a regular expression */
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

/* third filter that converts a string to a list of tokens */

/* finds the next caracter equals to car */
let find_next_char = (car, chaine) => {
  let rec aux = i => switch(i){
    |x when x >= String.length(chaine)-1 => None
    |x when chaine.[x] == car => Some(x)
    |x => aux(x+1)
  };
  aux(1);
};

/* finds the mathcing caracter equals to car, allowing nested patterns */
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

/* decrement an option(int) */
let decrement_opt = x => switch(x){
  |None => None
  |Some(x) => Some(x-1)
};

/* main function */
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

/* converts a string element in a token */
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

/* second filter */
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

/* code execution function */
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
