open Belt
open Utils

type setup = {
  board: Board.t,
  fealty: Fealty.t,
  selected: option<Tile.t>,
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
  | Select(Tile.t)
  | Reset
  | Shuffle
  | Start
  | Quit

// Returns a new Setup(...) that updates the selection state, arranges camped
// tiles onto the board, and re-arranges tiles that are already marshalled.
let arrange = ({board, fealty: player, selected, yours} as state: setup, tile: Tile.t) => {
  switch (selected, tile) {
  | (_, Corps({fealty})) when Fealty.opposes(player, fealty) => Setup({...state, selected: None})
  | (_, Field({vector})) when !Board.inBounds(player, vector) => Setup({...state, selected: None})
  | (_, Corps({status: Camped})) => Setup({...state, selected: Some(tile)})
  | (None, Corps(_)) => Setup({...state, selected: Some(tile)})

  | (Some(Corps({rank, status: Camped})), Field({terrain: Land, vector})) =>
    let status: Tile.status = Marshalled(vector)
    let tile: Tile.t = Corps({rank: rank, fealty: player, status: status})
    let board = Board.set(board, vector, tile)
    Setup({...state, board: board, selected: None, yours: remove(yours, rank)})

  | (Some(Corps({rank, status: Camped})), Corps({rank: replaced, status: Marshalled(vector)})) =>
    let status: Tile.status = Marshalled(vector)
    let tile: Tile.t = Corps({rank: rank, fealty: player, status: status})
    let board = Board.set(board, vector, tile)
    Setup({...state, board: board, selected: None, yours: list{replaced, ...remove(yours, rank)}})

  | (Some(Corps({status: Marshalled(src)})), Corps({status: Marshalled(dst)}))
  | (Some(Corps({status: Marshalled(src)})), Field({terrain: Land, vector: dst})) =>
    Setup({...state, board: Board.swap(board, src, dst), selected: None})

  | _ => Setup({...state, selected: None})
  }
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

let initialSetup = {
  board: Board.empty,
  fealty: Fealty.Red,
  selected: None,
  yours: Rank.starting,
  theirs: Rank.starting,
}

let resolve = (state, event) =>
  switch (state, event) {
  | (NotStarted, StartNew(fealty)) => Setup({...initialSetup, fealty: fealty})
  | (Setup(record), Select(tile)) => arrange(record, tile)
  | (Setup(record), Shuffle) => shuffle(record)
  | (Setup(record), Reset) => reset(record)
  | (Setup(_), Quit) => NotStarted
  | (Setup({board, fealty}), Start) =>
    Playing({board: board, fealty: fealty, turn: Fealty.Red, yours: list{}, theirs: list{}})
  | (_, _) => state
  }
