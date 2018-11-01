/* Slices a list */
let list_slice = (min, max, liste) => {
  let rec aux = (acc, i) => {
    i == max ? acc : aux([List.nth(liste, i), ...acc], i+1)
  };
  List.rev(aux([], min));
};

/* Same as List.nth but returns a default it failed */
let list_fetch = (liste, index, default) => {
  try(List.nth(liste, index)){
    |Invalid_argument("List.nth") => default;
    |Failure("nth") => default;
  };
};

/* Returns a copy of listeb with same size as liste.
Add default into new spots if necessary */
let list_same_size = (liste, listeb, default) => {
  List.mapi((index, _) => list_fetch(listeb, index, default), liste)
};

/* Return an Option(x) whith the firts x where f(x)
or None if not found */
let rec list_first = (f, liste) => switch(liste){
  |[] => None
  |[head, ..._] when f(head) => Some(head)
  |[_, ...tail] => list_first(f, tail)
}

/* Slices a string */
let string_slice = (min, max, chaine) => {
  let size = max - min;
  String.sub(chaine, min, size)
};

/* Aliias of string_of_int with failure coverage */
let super_int_of_string = chaine => try(int_of_string(chaine)){
  |Failure("int_of_string") => 0
}

/* Aliias of float_of_int with failure coverage */
let super_float_of_string = chaine => try(float_of_string(chaine)){
  |Failure("float_of_string") => 0.0
}
