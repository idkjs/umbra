module Umbra = {
  open Belt

  type side =
    | Red
    | Blue
    | None

  type kind =
    | Bomb
    | Mars
    | Genr
    | Crnl
    | Majr
    | Capt
    | Lieu
    | Serg
    | Mine
    | Scot
    | Spys
    | Flag

  type tile =
    | Army(side, kind)
    | Land
    | None

  let quantity = k =>
    switch k {
    | Bomb => 6
    | Mars => 1
    | Genr => 1
    | Crnl => 2
    | Majr => 3
    | Capt => 4
    | Lieu => 4
    | Serg => 4
    | Mine => 5
    | Scot => 8
    | Spys => 1
    | Flag => 1
    }

  let rank = k =>
    switch k {
    | Bomb => 11
    | Mars => 10
    | Genr => 9
    | Crnl => 8
    | Majr => 7
    | Capt => 6
    | Lieu => 5
    | Serg => 4
    | Mine => 3
    | Scot => 2
    | Spys => 1
    | Flag => 0
    }

  let captures = (a, b) =>
    switch (a, b) {
    | (Mine, Bomb) => true
    | (Spys, Mars) => true
    | _ => rank(a) > rank(b)
    }

  let flatMap = (xs, f) => Array.reduce(xs, [], (r, x) => Array.concat(r, f(x)))
  let kinds = [Bomb, Mars, Genr, Crnl, Majr, Capt, Lieu, Serg, Mine, Scot, Spys, Flag]
  let sides = [Red, Blue]
  let every = flatMap(sides, s => flatMap(kinds, k => Array.make(quantity(k), Army(s, k))))

  Js.log(every)
}
