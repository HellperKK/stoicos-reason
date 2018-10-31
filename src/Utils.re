let list_slice = (min, max, liste) => {
  let rec aux = (acc, i) => {
    i == max ? acc : aux([List.nth(liste, i), ...acc], i+1)
  };
  List.rev(aux([], min));
};

let list_fetch = (liste, index, default) => {
  try(List.nth(liste, index)){
    |Invalid_argument("List.nth") => default;
    |Failure("nth") => default;
  };
};

let list_same_size = (liste, listeb, default) => {
  List.mapi((index, _) => list_fetch(listeb, index, default), liste)
};

let string_slice = (min, max, chaine) => {
  let size = max - min;
  String.sub(chaine, min, size)
};

let super_int_of_string = chaine => try(int_of_string(chaine)){
  |Failure("int_of_string") => 0
}

let super_float_of_string = chaine => try(float_of_string(chaine)){
  |Failure("float_of_string") => 0.0
}
