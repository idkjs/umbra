open Belt
open Utils

type setup = {
  board: Board.t,
  fealty: Fealty.t,
  yours: list<Rank.t>,
  theirs: list<Rank.t>,
}

type playing = {
  board: Board.t,
  fealty: Fealty.t,
  turn: Fealty.t,
  yours: list<Rank.t>,
  theirs: list<Rank.t>,
}

type t =
  | NotStarted
  | Setup(setup)
  | Playing(playing)

type event =
  | StartNew(Fealty.t)
  | Arrange(Rank.t, Vector.t)
  | Rearrange(Vector.t, Vector.t)
  | Reset
  | Shuffle
  | Start
  | Quit

// Returns a new Setup(...) with the given `rank` placed on the board at the
// position given by `coords`.
let arrange = ({board, fealty, yours} as state: setup, rank, coords) =>
  switch board[Board.idx(coords)] {
  | None => Setup(state)
  | Some(_) when !Board.isStartingTile(fealty, coords) => Setup(state)
  | Some(Field({terrain: None})) => Setup(state)
  | Some(Field(_)) =>
    let status: Tile.status = Marshalled(coords)
    let tile: Tile.t = Corps({rank: rank, fealty: fealty, status: status})
    Setup({...state, yours: remove(yours, rank), board: Board.set(board, coords, tile)})

  | Some(Corps({rank: replaced})) =>
    let status: Tile.status = Marshalled(coords)
    let tile: Tile.t = Corps({rank: rank, fealty: fealty, status: status})
    Setup({
      ...state,
      yours: list{replaced, ...remove(yours, rank)},
      board: Board.set(board, coords, tile),
    })
  }

// Returns a new Setup(...) with the tiles at `src` and `dst` swapped.
let rearrange = ({board, fealty, yours} as state: setup, src, dst) =>
  switch (board[Board.idx(src)], board[Board.idx(dst)]) {
  | (Some(a), Some(b)) when Board.isStartingTile(fealty, dst) =>
    switch (a, b) {
    | (_, Field({terrain: None})) => Setup(state)
    | (Corps({fealty: h}), Corps({fealty: k})) when Fealty.opposes(h, k) => Setup(state)
    | (_, _) => Setup({...state, board: Board.swap(board, src, dst)})
    }
  | _ => Setup(state)
  }

// Returns a new Setup(...) with any arranged tiles returned to camp.
let reset = ({board, fealty} as state: setup) => {
  let rec reset = (board: Board.t, indices: list<int>) =>
    switch indices {
    | list{} => board
    | list{index, ...indices} =>
      switch board[index] {
      | Some(_) => {
          Belt.Array.setUnsafe(board, index, Field({terrain: Land, vector: Board.vec(index)}))
          reset(board, indices)
        }
      | None => board
      }
    }

  let indices = fealty->Board.indices->List.fromArray
  let updated = reset(Array.copy(board), indices)
  Setup({...state, board: updated, yours: Rank.starting})
}

// Returns a new Setup(...) that randomly arranges all remaining camped tiles
// on the board.
let shuffle = ({board, fealty, yours} as state: setup) => {
  let rec occupy = (board: Board.t, indexes, ranks) =>
    switch indexes {
    | list{} => board
    | list{index, ...indexes} =>
      switch board[index] {
      | Some(Field(_)) =>
        switch ranks {
        | list{} => board
        | list{rank, ...ranks} =>
          let status: Tile.status = Marshalled(Board.vec(index))
          let tile: Tile.t = Corps({rank: rank, fealty: fealty, status: status})
          Belt.Array.setUnsafe(board, index, tile)
          occupy(board, indexes, ranks)
        }
      | _ => occupy(board, indexes, ranks)
      }
    }

  let shuffled = fealty->Board.indices->Array.shuffle->List.fromArray
  let updated = occupy(Array.copy(board), shuffled, yours)
  Setup({...state, board: updated, yours: list{}})
}

let resolve = (state, event) =>
  switch (state, event) {
  | (NotStarted, StartNew(fealty)) =>
    Setup({board: Board.empty, fealty: fealty, yours: Rank.starting, theirs: Rank.starting})
  | (Setup(record), Arrange(rank, dst)) => arrange(record, rank, dst)
  | (Setup(record), Rearrange(src, dst)) => rearrange(record, src, dst)
  | (Setup(record), Shuffle) => shuffle(record)
  | (Setup(record), Reset) => reset(record)
  | (Setup(_), Quit) => NotStarted
  | (Setup({board, fealty}), Start) =>
    Playing({board: board, fealty: fealty, turn: Fealty.Red, yours: list{}, theirs: list{}})
  | (_, _) => state
  }
