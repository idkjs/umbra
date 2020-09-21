open Belt
open Utils

type event =
  | StartNew
  | Arrange(Rank.t, Vector.t)
  | Rearrange(Vector.t, Vector.t)

type state =
  | Invalid
  | NotStarted
  | Setup({board: Board.t, fealty: Fealty.t, yours: list<Rank.t>, theirs: list<Rank.t>})

let resolve = (state, event) =>
  switch (state, event) {
  | (NotStarted, StartNew) =>
    Setup({
      board: Board.empty,
      fealty: Fealty.starting,
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

  | _ => state
  }
