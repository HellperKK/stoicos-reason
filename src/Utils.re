let list_slice = (min, max, liste) => {
  let rec aux = (acc, i) => {
    i == max ? acc : aux([List.nth(liste, i), ...acc], i+1)
  };
  List.rev(aux([], min));
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
