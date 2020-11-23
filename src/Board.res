open Belt

type t = array<Tile.t>

type cardinal = N | E | S | W

let s = "
  Flag Spys Scou Scou Scou Scou Scou Scou Scou Scou
  Mine Mine Mine Mine Mine Serg Serg Serg Serg Lieu
  Lieu Lieu Lieu Capt Capt Capt Capt Majr Majr Majr
  Crnl Crnl Genr Mars Bomb Bomb Bomb Bomb Bomb Bomb
  Land Land Land Land Land Land Land Land Land Land
  Land Land Land Land Land Land Land Land Land Land
  Land Land Land Land Land Land Land Land Land Land
  Land Land Land Land Land Land Land Land Land Land
  Land Land None None Land Land None None Land Land
  Land Land None None Land Land None None Land Land
  Land Land Land Land Land Land Land Land Land Land
  Land Land Land Land Land Land Land Land Land Land
  Land Land Land Land Land Land Land Land Land Land
  Land Land Land Land Land Land Land Land Land Land
  Flag Spys Scou Scou Scou Scou Scou Scou Scou Scou
  Mine Mine Mine Mine Mine Serg Serg Serg Serg Lieu
  Lieu Lieu Lieu Capt Capt Capt Capt Majr Majr Majr
  Crnl Crnl Genr Mars Bomb Bomb Bomb Bomb Bomb Bomb
"

Js.String.replaceByRe(%re("/\s\s+/g"), " ", s)->Js.String.trim->Js.log
// Js.String.replaceByRe(%re("/[aeiou]/g"), "x", "vowels be gone") == "vxwxls bx gxnx"

let vec = index => {
  (mod(index, 10), index / 10)
}

let idx = ((x, y)) => {
  y * 10 + x
}

let unmarshal = (index, name): Tile.t => {
  switch (vec(index), name) {
  | ((_, _) as coord, "Land") => Field({terrain: Land, vector: coord})
  | ((_, _) as coord, "None") => Field({terrain: None, vector: coord})
  | ((_, y) as coord, rank) => {
      let fealty: Fealty.t = y <= 4 ? Blue : Red
      switch rank {
      | "Bomb" => Corps({rank: Bomb, fealty: Blue, status: Camped, vector: coord})
      | "Mars" => Corps({rank: Mars, fealty: Blue, status: Camped, vector: coord})
      | "Genr" => Corps({rank: Genr, fealty: Blue, status: Camped, vector: coord})
      | "Crnl" => Corps({rank: Crnl, fealty: Blue, status: Camped, vector: coord})
      | "Majr" => Corps({rank: Majr, fealty: Blue, status: Camped, vector: coord})
      | "Capt" => Corps({rank: Capt, fealty: Blue, status: Camped, vector: coord})
      | "Lieu" => Corps({rank: Lieu, fealty: Blue, status: Camped, vector: coord})
      | "Serg" => Corps({rank: Serg, fealty: Blue, status: Camped, vector: coord})
      | "Mine" => Corps({rank: Mine, fealty: Blue, status: Camped, vector: coord})
      | "Scou" => Corps({rank: Scot, fealty: Blue, status: Camped, vector: coord})
      | "Spys" => Corps({rank: Spys, fealty: Blue, status: Camped, vector: coord})
      | "Flag" => Corps({rank: Flag, fealty: Blue, status: Camped, vector: coord})
      }
    }
  | _ => Field({terrain: Land, vector: (0, 0)})
  }
}

unmarshal(0, "Flag")->Js.log

let update = (tile: Tile.t, vector: Vector.t): Tile.t => {
  switch tile {
  | Field(_ as data) => Field({...data, vector: vector})
  | Corps(_ as data) => Corps({...data, vector: vector})
  }
}

let set = (board, vector, tile) => {
  let copy = Belt.Array.copy(board)
  Belt.Array.setUnsafe(copy, idx(vector), update(tile, vector))
  copy
}

let swap = (board, src, dst) => {
  let a = idx(src)
  let b = idx(dst)
  switch (board[a], board[b]) {
  | (None, None) | (_, None) | (None, _) => board
  | (Some(h), Some(k)) => {
      let copy = Belt.Array.copy(board)
      Belt.Array.setUnsafe(copy, a, update(k, src))
      Belt.Array.setUnsafe(copy, b, update(h, dst))
      copy
    }
  }
}

let offset = dir =>
  switch dir {
  | N => -10
  | E => 1
  | S => 10
  | W => -1
  }

let path = (src, rank: Rank.t, card) =>
  switch (vec(src), rank, card) {
  | ((_, 0), _, N) => list{}
  | ((9, _), _, E) => list{}
  | ((_, 9), _, S) => list{}
  | ((0, _), _, W) => list{}
  | ((x, y), Scot, _) => {
      let (backwards, end) = {
        switch card {
        | N => (S, idx((x, 0)))
        | E => (W, idx((9, y)))
        | S => (N, idx((x, 9)))
        | W => (E, idx((0, y)))
        }
      }

      let rec step = (xs, current) => {
        current == src ? xs : step(list{current, ...xs}, current + offset(backwards))
      }

      step(list{}, end)
    }

  | _ => list{src + offset(card)}
  }

let moves = (board: t, index) => {
  switch board[index] {
  | None => Set.Int.empty
  | Some(Field(_)) | Some(Corps({rank: Bomb})) | Some(Corps({rank: Flag})) => Set.Int.empty
  | Some(Corps({rank}) as src) => {
      let rec step = (rs, indexes) =>
        switch indexes {
        | list{} => rs
        | list{hd, ...tl} =>
          switch board[hd] {
          | None => rs
          | Some(dst) =>
            switch Tile.walk(src, dst) {
            | (true, true) => step(Set.Int.add(rs, hd), tl)
            | (true, false) => Set.Int.add(rs, hd)
            | (false, _) => rs
            }
          }
        }

      let rec hike = (rs, dirs) =>
        switch dirs {
        | list{} => rs
        | list{hd, ...tl} => hike(step(rs, path(index, rank, hd)), tl)
        }

      hike(Set.Int.empty, list{N, E, S, W})
    }
  }
}

let initial = (index): Tile.t => {
  let vector = vec(index)
  switch index {
  | index when index < 40 => Field({terrain: Land, vector: vector})
  | index when index < 42 => Field({terrain: Land, vector: vector})
  | index when index < 44 => Field({terrain: None, vector: vector})
  | index when index < 46 => Field({terrain: Land, vector: vector})
  | index when index < 48 => Field({terrain: None, vector: vector})
  | index when index < 50 => Field({terrain: Land, vector: vector})
  | index when index < 52 => Field({terrain: Land, vector: vector})
  | index when index < 54 => Field({terrain: None, vector: vector})
  | index when index < 56 => Field({terrain: Land, vector: vector})
  | index when index < 58 => Field({terrain: None, vector: vector})
  | _ => Field({terrain: Land, vector: vector})
  }
}

let indices = (fealty: Fealty.t) =>
  switch fealty {
  | Blue => Array.range(0, 39)
  | Red => Array.range(60, 99)
  }

let empty = Array.range(0, 99)->Array.map(initial)

let inBounds = (fealty: Fealty.t, (x, y)) =>
  x >= 0 &&
  x <= 9 &&
  switch fealty {
  | Red => y < 10 && y > 5
  | Blue => y < 4 && y >= 0
  }
