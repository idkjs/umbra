open Belt;
open Utils;

type setup = {
  board: Board.t,
  fealty: Fealty.t,
  camps: Camps.t,
  selected: option(Tile.t),
};

type playing = {
  board: Board.t,
  fealty: Fealty.t,
  camps: Camps.t,
  selected: option(Tile.t),
  turn: Fealty.t,
};

type t =
  | NotStarted
  | Setup(setup)
  | Playing(playing);

type event =
  | StartNew(Fealty.t)
  | Select(Tile.t)
  | Reset
  | Shuffle
  | Start
  | Quit;

/* Returns a new setup state with the given fealty as the player's chosen side.*/
let create = fealty => {
  board: Board.empty,
  fealty,
  selected: None,
  camps: Camps.initial,
};

/* Returns a new state that updates the selection state, arranges camped tiles*/
/* onto the board, and re-arranges tiles that are already marshalled.*/
let arrange =
    ({board, fealty: player, selected, camps} as state: setup, tile: Tile.t) => {
  let update = Camps.update(camps, player);

  switch (selected, tile) {
  | (_, Corps({fealty})) when Fealty.opposes(player, fealty) => {
      ...state,
      selected: None,
    }
  | (_, Field({vector})) when !Board.inBounds(player, vector) => {
      ...state,
      selected: None,
    }
  | (_, Corps({status: Camped})) => {...state, selected: Some(tile)}
  | (None, Corps(_)) => {...state, selected: Some(tile)}

  | (Some(Corps({rank, status: Camped})), Field({terrain: Land, vector})) =>
    let status: Tile.status = Marshalled(vector);
    let tile: Tile.t = Corps({rank, fealty: player, status});
    let board = Board.set(board, vector, tile);
    {
      ...state,
      board,
      selected: None,
      camps: update(camp => remove(camp, rank)),
    };

  | (
      Some(Corps({rank, status: Camped})),
      Corps({rank: r, status: Marshalled(vector)}),
    ) =>
    let status: Tile.status = Marshalled(vector);
    let tile: Tile.t = Corps({rank, fealty: player, status});
    let board = Board.set(board, vector, tile);
    {
      ...state,
      board,
      selected: None,
      camps: update(camp => [r, ...remove(camp, rank)]),
    };

  | (
      Some(Corps({status: Marshalled(src)})),
      Corps({status: Marshalled(dst)}),
    )
  | (
      Some(Corps({status: Marshalled(src)})),
      Field({terrain: Land, vector: dst}),
    ) =>
    let board = Board.swap(board, src, dst);
    {...state, board, selected: None};

  | _ => {...state, selected: None}
  };
};

/* Returns a new state that randomly arranges all remaining camped tiles on the*/
/* board.*/
let shuffle = ({board, camps} as state: setup, fealty: Fealty.t) => {
  let update = Camps.update(camps, fealty);

  let rec occupy = (board: Board.t, indexes, ranks) =>
    switch (indexes) {
    | [] => board
    | [index, ...indexes] =>
      switch (board[index]) {
      | Some(Field(_)) =>
        switch (ranks) {
        | [] => board
        | [rank, ...ranks] =>
          let status: Tile.status = Marshalled(Board.vec(index));
          let tile: Tile.t = Corps({rank, fealty, status});
          Belt.Array.setUnsafe(board, index, tile);
          occupy(board, indexes, ranks);
        }
      | _ => occupy(board, indexes, ranks)
      }
    };

  let shuffled = fealty->Board.indices->Array.shuffle->List.fromArray;
  let updated =
    occupy(Array.copy(board), shuffled, Camps.get(camps, fealty));
  {...state, board: updated, camps: update(_ => [])};
};

/* Returns a new state with any arranged tiles returned to camp.*/
let reset = ({board, fealty, camps} as state: setup) => {
  let update = Camps.update(camps, fealty);

  let rec reset = (board: Board.t, indices: list(int)) =>
    switch (indices) {
    | [] => board
    | [index, ...indices] =>
      switch (board[index]) {
      | Some(_) =>
        Belt.Array.setUnsafe(
          board,
          index,
          Field({terrain: Land, vector: Board.vec(index)}),
        );
        reset(board, indices);

      | None => board
      }
    };

  let indices = fealty->Board.indices->List.fromArray;
  let updated = reset(Array.copy(board), indices);
  {...state, board: updated, camps: update(_ => Rank.starting)};
};

/* Returns a new playing state with the opposing tiles randomly arranged on*/
/* the board.*/
let start = ({fealty} as state: setup) => {
  let shuffled = shuffle(state, Fealty.opposing(fealty));
  {
    board: shuffled.board,
    fealty,
    camps: Camps.empty,
    selected: None,
    turn: Fealty.Red,
  };
};

/* Returns a new playing state with a modified `selected` state depending on*/
/* the given tile selection.*/
// let select = (state: playing, tile: Tile.t) => state;
let select = (state: playing, tile: option(Tile.t)) => {
  switch (tile) {
  | Some(tile) => {...state, selected: Some(tile)}
  | None => {...state, selected: None}
  };
};

let resolve = (state, event) =>
  switch (state, event) {
  | (NotStarted, StartNew(fealty)) => Setup(create(fealty))
  | (Setup(data), Select(tile)) => Setup(arrange(data, tile))
  | (Setup({fealty} as data), Shuffle) => Setup(shuffle(data, fealty))
  | (Setup(data), Reset) => Setup(reset(data))
  | (Setup(_), Quit) => NotStarted
  | (Setup(data), Start) => Playing(start(data))
  | (Playing({selected: None} as data), Select(tile)) =>
    Playing(select(data, Some(tile)))
  | (_, _) => state
  };
