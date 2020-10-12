type t =
  | Red
  | Blue

let starting = Red

let opposes = (a, b) =>
  switch (a, b) {
  | (Red, Red) | (Blue, Blue) => false
  | (Red, Blue) | (Blue, Red) => true
  }

let opposing = fealty =>
  switch fealty {
  | Red => Blue
  | Blue => Red
  }
