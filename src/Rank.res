open Utils

type t =
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

let quantity = rank =>
  switch rank {
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

let strength = rank =>
  switch rank {
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

let starting = {
  let ranks = list{Bomb, Mars, Genr, Crnl, Majr, Capt, Lieu, Serg, Mine, Scot, Spys, Flag}

  let rec recur = (xs, ks) => {
    switch ks {
    | list{} => xs
    | list{hd, ...tl} => recur(conj(xs, repeat(hd, quantity(hd))), tl)
    }
  }

  recur(list{}, ranks)
}

let captures = (atk, def) =>
  switch (atk, def) {
  | (Mine, Bomb) => (false, true)
  | (Spys, Mars) => (false, true)
  | (_, Bomb) => (true, false)
  | (h, k) when h == k => (true, true)
  | (_, _) => (false, strength(atk) > strength(def))
  }
