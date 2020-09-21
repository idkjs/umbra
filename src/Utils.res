let rec remove = (xs, x) =>
  switch xs {
  | list{} => xs
  | list{hd, ...tl} => hd === x ? tl : list{hd, ...remove(tl, x)}
  }

let repeat = (x, t) => {
  let rec recur = (xs, x, t) => {
    t == 1 ? list{x, ...xs} : recur(list{x, ...xs}, x, t - 1)
  }

  recur(list{}, x, t)
}

let rec conj = (xs, ys) =>
  switch ys {
  | list{} => xs
  | list{hd, ...tl} => conj(list{hd, ...xs}, tl)
  }
