type t('a, 'b) =
  |Leaf
  |Branch(('a, 'b), t('a, 'b), t('a, 'b));

let empty = Leaf;
let singleton = (key, value) => Branch((key, value), Leaf, Leaf);

let rec mem = (hash, key) => switch(hash){
  |Leaf => false;
  |Branch((k, _), _, _) when k == key => true;
  |Branch((k, _), left, _) when k > key => mem(left, key)
  |Branch(_, _, right) => mem(right, key)
};

let rec find_opt = (hash, key) => switch(hash){
  |Leaf => None;
  |Branch((k, v), _, _) when k == key => Some(v);
  |Branch((k, _), left, _) when k > key => find_opt(left, key)
  |Branch(_, _, right) => find_opt(right, key)
};

let rec add = (hash, key, value) => switch(hash){
  |Leaf => Branch((key, value), Leaf, Leaf);
  |Branch((k, v), left, right) when k == key => Branch((k, v), left, right);
  |Branch((k, v), left, right) when k > key => Branch((k, v), add(left, key, value), right);
  |Branch((k, v), left, right) => Branch((k, v), left, add(right, key, value));
};

let rec replace = (hash, key, value) => switch(hash){
  |Leaf => Branch((key, value), Leaf, Leaf);
  |Branch((k, _), left, right) when k == key => Branch((key, value), left, right);
  |Branch((k, v), left, right) when k > key => Branch((k, v), replace(left, key, value), right);
  |Branch((k, v), left, right) => Branch((k, v), left, replace(right, key, value));
};

let rec fold_left = (hash, value, func) => switch(hash){
  |Leaf => value;
  |Branch((k, v), left, right) => {
    let result = func(value, k, v);
    let temp = fold_left(left, result, func);
    fold_left(right, temp, func);
  };
};

let pairs = hash => fold_left(hash, [], (memo, k, v) => [(k, v), ...memo])
