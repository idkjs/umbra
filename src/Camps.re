type t = {
  blue: list(Rank.t),
  red: list(Rank.t),
};

let empty: t = {blue: [], red: []};

let initial: t = {blue: Rank.starting, red: Rank.starting};

let get = (camps, fealty: Fealty.t) =>
  switch (fealty) {
  | Blue => camps.blue
  | Red => camps.red
  };

let update = (camps, fealty: Fealty.t, fn: list(Rank.t) => list(Rank.t)) =>
  switch (fealty) {
  | Blue => {...camps, blue: fn(camps.blue)}
  | Red => {...camps, red: fn(camps.red)}
  };
