open Belt
open Utils

type t = array<Tile.t>

type cardinal = N | E | S | W

let vec = index => {
  (mod(index, 10), index / 10)
}

let idx = ((x, y)) => {
  y * 10 + x
}

let update = (tile: Tile.t, vector: Vector.t): Tile.t => {
  switch tile {
  | Field(_ as data) => Field({...data, vector: vector})
  | Corps({status: Marshalled(_)} as data) => Corps({...data, status: Marshalled(vector)})
  | _ => tile
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

let moves = (board, index) => {
  switch (board[index]: option<Tile.t>) {
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

let empty = Array.range(0, 99)->Array.map(initial)

let isStartingTile = (fealty: Fealty.t, (x, y)) =>
  x >= 0 &&
  x <= 9 &&
  switch fealty {
  | Red => y < 10 && y > 5
  | Blue => y < 4 && y >= 0
  }
