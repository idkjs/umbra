type t = (int, int);

let eq = ((ax, ay): t, (bx, by): t) => ax == bx && ay == by;
