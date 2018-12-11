/* Module for an Immutable Hash */

type t('a, 'b) =
  |Leaf
  |Branch(('a, 'b), t('a, 'b), t('a, 'b));

let empty = Leaf;
let singleton = (key, value) => Branch((key, value), Leaf, Leaf);

let rec mem = (key, hash) => switch(hash){
  |Leaf => false;
  |Branch((k, _), _, _) when k == key => true;
  |Branch((k, _), left, _) when k > key => mem(key, left)
  |Branch(_, _, right) => mem(key, right)
};

let rec find_opt = (key, hash) => switch(hash){
  |Leaf => None;
  |Branch((k, v), _, _) when k == key => Some(v);
  |Branch((k, _), left, _) when k > key => find_opt(key, left)
  |Branch(_, _, right) => find_opt(key, right)
};

let rec add = (key, value, hash) => switch(hash){
  |Leaf => Branch((key, value), Leaf, Leaf);
  |Branch((k, v), left, right) when k == key => Branch((k, v), left, right);
  |Branch((k, v), left, right) when k > key => Branch((k, v), add(key, value, left), right);
  |Branch((k, v), left, right) => Branch((k, v), left, add(key, value, right));
};

let rec replace = (key, value, hash) => switch(hash){
  |Leaf => Branch((key, value), Leaf, Leaf);
  |Branch((k, v), left, right) when k == key => Branch((k, v), left, right);
  |Branch((k, v), left, right) when k > key => Branch((k, v), replace(key, value, left), right);
  |Branch((k, v), left, right) => Branch((k, v), left, replace(key, value, right));
};

let rec fold_left = (value, func, hash) => switch(hash){
  |Leaf => value;
  |Branch((k, v), left, right) => {
    let result = func(value, k, v);
    let temp = fold_left(result, func, left);
    fold_left(temp, func, right);
  };
};

let pairs = hash => {
  let temp = fold_left([], (memo, k, v) => [(k, v), ...memo], hash);
  List.rev(temp);
};

let keys = hash => {
  let temp = fold_left([], (memo, k, _) => [k, ...memo], hash);
  List.rev(temp);
};

let values = hash => {
  let temp = fold_left([], (memo, _, v) => [v, ...memo], hash);
  List.rev(temp);
};

let test = () => {
  let test_val:t(string, int) = empty
  |> add("hello", 1)
  |> add("john", 2)
  |> add("doe", 3)
  |> add("man", 4);
  Js.log(test_val);
}
