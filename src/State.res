open Belt

let flatMap = (xs, f) => {
  Array.reduce(xs, [], (r, x) => Array.concat(r, f(x)))
}

type direction =
  | N
  | E
  | S
  | W

type side =
  | Red
  | Blue

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
  | None
  | Land
  | Army(side, kind)

type mode =
  | Pending
  | Setup
  | Play
  | Over

// Returns the index in the board array represented by the given coordinates.
let index = ((x, y)) => {
  y * 10 + x
}

// Returns a two-element tuple of (x, y) that represent the cartesian
// coordinates of the given board index. The top-left most tile in the board
// is represented by (0, 0).
let coords = i => {
  (mod(i, 10), i / 10)
}

// Returns the number of pieces of the given kind that each player can
// distribute on the board during setup.
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

// Returns the rank of the given kind as an integer. The rank represents the
// attack rank of the piece, determining the result of an attack.
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

// Returns the result of an attack, a two-element tuple of booleans. The
// first boolean represents whether or not the attacking piece will be
// captured as a result of the attack; the second whether the defending
// piece will be captured.
let captures = (a, b) =>
  switch (a, b) {
  | (Mine, Bomb) => (false, true)
  | (Spys, Mars) => (false, true)
  | (_, Bomb) => (true, false)
  | (h, k) when h == k => (true, true)
  | (_, _) => (false, rank(a) > rank(b))
  }

// Returns the set of board indices that the tile given by the index can
// move to in the given direction. This only considers board boundaries
// and not the tile contents themselves.
let path = (index, kind, dir) =>
  switch (coords(index), kind, dir) {
  | ((_, 0), _, N) => []
  | ((9, _), _, E) => []
  | ((_, 9), _, S) => []
  | ((0, _), _, W) => []
  | ((_, y), Scot, N) => Array.map(Array.range(1, y), x => index - x * 10)
  | ((x, _), Scot, E) => Array.map(Array.range(1, x - 1), x => index + x)
  | ((_, y), Scot, S) => Array.map(Array.range(1, 9 - y), x => index + x * 10)
  | ((x, _), Scot, W) => Array.map(Array.range(1, x), x => index - x)
  | (_, _, N) => [index - 10]
  | (_, _, E) => [index + 1]
  | (_, _, S) => [index + 10]
  | (_, _, W) => [index - 1]
  }

// Returns the result of moving the tile `a` to the tile `b` as a two-element
// tuple of booleans. The first boolean is true if the moving tile `a` can
// legally move into the space occupied by `b`. The second boolean is true
// if the piece can continue moving in that direction or if it should stop
// there altogether.
let walk = (a, b) =>
  switch (a, b) {
  | (Army(h, _), Army(k, _)) => (h !== k, false)
  | (Army(_, _), Land) => (true, true)
  | (_, _) => (false, false)
  }

// Returns the set of all legal positions in the board that the tile found
// at the given index can perform. The returned positions are represented
// by the board index.
let moves = (board, index) =>
  switch board[index] {
  | Some(Army(_, Bomb)) => []
  | Some(Army(_, Flag)) => []
  | Some(Army(side, kind)) => {
      let dirs = [N, E, S, W]
      let initial = ([], true)
      flatMap(dirs, dir => {
        let candidates = path(index, kind, dir)
        let (valid, _) = Array.reduce(candidates, initial, ((result, continue), index) => {
          if continue {
            switch board[index] {
            | Some(tile) =>
              switch walk(Army(side, kind), tile) {
              | (true, cont) => (Array.concat(result, [index]), cont)
              | (false, _) => (result, false)
              }
            | None => (result, false)
            }
          } else {
            (result, continue)
          }
        })

        valid
      })
    }
  | _ => []
  }

type state = {
  mode: mode,
  turn: side,
  tiles: array<tile>,
}

let createNewState = _ => {
  mode: Pending,
  turn: Red,
  tiles: Array.concatMany([
    Array.make(40, Land),
    Array.make(2, Land),
    Array.make(2, None),
    Array.make(2, Land),
    Array.make(2, None),
    Array.make(2, Land),
    Array.make(2, Land),
    Array.make(2, None),
    Array.make(2, Land),
    Array.make(2, None),
    Array.make(2, Land),
    Array.make(40, Land),
  ]),
}

let newSet = s => {
  let kinds = [Bomb, Mars, Genr, Crnl, Majr, Capt, Lieu, Serg, Mine, Scot, Spys, Flag]
  flatMap(kinds, k => Array.make(quantity(k), Army(s, k)))
}

// Returns a new board with random piece positions.
let newBoard = _ => {
  let lts = [Land, Land, None, None, Land, Land, None, None, Land, Land]
  let bts = Array.shuffle(newSet(Blue))
  let rts = Array.shuffle(newSet(Red))
  Array.concatMany([bts, lts, lts, rts])
}
