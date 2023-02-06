use aiken/list

/// Sort list by a certain predicate.
pub fn sort_by(xs: List<a>, f: fn(a, a) -> Int) -> List<a> {
  do_merge_all(do_sequences(xs, f), f)
}

fn do_sequences(xs: List<a>, f: fn(a, a) -> Int) -> List<List<a>> {
  when xs is {
    [x, y, ..rest] ->
      if f(x, y) > 0 {
        do_descending(y, [x], rest, f)
      } else {
        do_ascending(y, [x], rest, f)
      }
    xs -> [xs]
  }
}

fn do_descending(
  x: a,
  xs: List<a>,
  rest: List<a>,
  f: fn(a, a) -> Int,
) -> List<List<a>> {
  let [y, ..ys] = rest
  if f(x, y) > 0 {
    do_descending(y, [x, ..xs], ys, f)
  } else {
    [[x, ..xs], ..do_sequences(rest, f)]
  }
}

fn do_ascending(
  x: a,
  xs: List<a>,
  rest: List<a>,
  f: fn(a, a) -> Int,
) -> List<List<a>> {
  let [y, ..ys] = rest
  if f(x, y) <= 0 {
    do_ascending(y, list.concat(xs, [x]), ys, f)
  } else {
    [list.concat(xs, [x]), ..do_sequences(rest, f)]
  }
}

fn do_merge(xs: List<a>, ys: List<a>, f: fn(a, a) -> Int) -> List<a> {
  when [xs, ys] is {
    [[], ys] -> ys
    [xs, []] -> xs
    [[x, ..xs2], [y, ..ys2]] ->
      if f(x, y) > 0 {
        [y, ..do_merge(xs, ys2, f)]
      } else {
        [x, ..do_merge(xs2, ys, f)]
      }
  }
}

fn do_merge_pairs(xs: List<List<a>>, f: fn(a, a) -> Int) -> List<List<a>> {
  when xs is {
    [x, y, ..rest] -> [do_merge(x, y, f), ..do_merge_pairs(rest, f)]
    xs -> xs
  }
}

fn do_merge_all(xs: List<List<a>>, f: fn(a, a) -> Int) -> List<a> {
  when xs is {
    [x] -> x
    xs -> do_merge_all(do_merge_pairs(xs, f), f)
  }
}
// test sort_by_1() {
//   let xs = [[4, 3, 2, 1], [2, 3, 4, 5]]
//   when xs is {
//     [[x, xs2], [y, ys2]] -> True
//   }
// }
