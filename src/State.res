open Belt
open Utils

type t =
  | NotStarted
  | Setup({board: Board.t, fealty: Fealty.t, yours: list<Rank.t>, theirs: list<Rank.t>})
  | Playing({board: Board.t, fealty: Fealty.t})

type event =
  | StartNew(Fealty.t)
  | Arrange(Rank.t, Vector.t)
  | Rearrange(Vector.t, Vector.t)
  | Shuffle
  | Start

let resolve = (state, event) =>
  switch (state, event) {
  | (NotStarted, StartNew(fealty)) =>
    Setup({
      board: Board.empty,
      fealty: fealty,
      yours: Rank.starting,
      theirs: Rank.starting,
    })

  | (Setup({board, fealty: player, yours} as record), Arrange(rank, dst)) => {
      let _ = ()
      switch board[Board.idx(dst)] {
      | None => state
      | Some(_) when !Board.isStartingTile(player, dst) => state
      | Some(Field({terrain: None})) => state
      | Some(Field(_)) =>
        Setup({
          ...record,
          yours: remove(yours, rank),
          board: Board.set(
            board,
            dst,
            Corps({rank: rank, fealty: player, status: Marshalled(dst)}),
          ),
        })

      | Some(Corps({rank: replaced})) =>
        Setup({
          ...record,
          yours: list{replaced, ...remove(yours, rank)},
          board: Board.set(
            board,
            dst,
            Corps({rank: rank, fealty: player, status: Marshalled(dst)}),
          ),
        })
      }
    }

  | (Setup({board, fealty: player} as record), Rearrange(src, dst)) => {
      let _ = ()
      switch (board[Board.idx(src)], board[Board.idx(dst)]) {
      | (Some(a), Some(b)) when Board.isStartingTile(player, dst) =>
        switch (a, b) {
        | (_, Field({terrain: None})) => state
        | (Corps({fealty: h}), Corps({fealty: k})) when Fealty.opposes(h, k) => state
        | (_, _) => Setup({...record, board: Board.swap(board, src, dst)})
        }
      | _ => state
      }
    }

  | (Setup({board, fealty, yours} as record), Shuffle) => {
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

      let indexes = switch fealty {
      | Blue => Array.range(0, 39)
      | Red => Array.range(60, 99)
      }

      let shuffled = List.fromArray(Array.shuffle(indexes))
      let updated = occupy(Array.copy(board), shuffled, yours)

      Setup({...record, board: updated, yours: list{}})
    }

  | (_, _) => state
  }
