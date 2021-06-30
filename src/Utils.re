let rec remove = (xs, x) =>
  switch (xs) {
  | [] => xs
  | [hd, ...tl] =>
    if (hd === x) {
      tl;
    } else {
      [hd, ...remove(tl, x)];
    }
  };

let repeat = (x, t) => {
  let rec recur = (xs, x, t) =>
    if (t == 1) {
      [x, ...xs];
    } else {
      recur([x, ...xs], x, t - 1);
    };

  recur([], x, t);
};

let rec conj = (xs, ys) =>
  switch (ys) {
  | [] => xs
  | [hd, ...tl] => conj([hd, ...xs], tl)
  };
