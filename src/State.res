open Belt

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

let assoc = (xs, k, v) => {
  let c = Array.copy(xs)
  Array.setUnsafe(c, k, v)
  c
}

let rec remove = (xs, x) =>
  switch xs {
  | list{} => xs
  | list{hd, ...tl} => hd === x ? tl : list{hd, ...remove(tl, x)}
  }

let repeat = (x, t) => {
  let rec recur = (xs, x, t) => {
    t == 1 ? list{x, ...xs} : recur(list{x, ...xs}, x, t - 1)
  }

  recur(list{}, x, t)
}

let rec conj = (xs, ys) =>
  switch ys {
  | list{} => xs
  | list{hd, ...tl} => conj(list{hd, ...xs}, tl)
  }

let index = ((x, y)) => {
  y * 10 + x
}

let coords = i => {
  (mod(i, 10), i / 10)
}

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
  | (Mine, Bomb) => (false, true)
  | (Spys, Mars) => (false, true)
  | (_, Bomb) => (true, false)
  | (h, k) when h == k => (true, true)
  | (_, _) => (false, rank(a) > rank(b))
  }

let validStart = (s, (x, y)) =>
  x >= 0 &&
  x <= 9 &&
  switch s {
  | Red => y < 10 && y > 5
  | Blue => y < 4 && y > 0
  }

let offset = dir =>
  switch dir {
  | N => -10
  | E => 1
  | S => 10
  | W => -1
  }

let path = (start, kind, dir) =>
  switch (coords(start), kind, dir) {
  | ((_, 0), _, N) => list{}
  | ((9, _), _, E) => list{}
  | ((_, 9), _, S) => list{}
  | ((0, _), _, W) => list{}
  | ((x, y), Scot, _) => {
      let (backwards, end) = {
        switch dir {
        | N => (S, index((x, 0)))
        | E => (W, index((9, y)))
        | S => (N, index((x, 9)))
        | W => (E, index((0, y)))
        }
      }

      let rec step = (xs, current) => {
        current == start ? xs : step(list{current, ...xs}, current + offset(backwards))
      }

      step(list{}, end)
    }

  | _ => list{start + offset(dir)}
  }

let walk = (a, b) =>
  switch (a, b) {
  | (Army(h, _), Army(k, _)) => (h !== k, false)
  | (Army(_, _), Land) => (true, true)
  | (_, _) => (false, false)
  }

let moves = (board, index) => {
  switch board[index] {
  | None => Set.Int.empty
  | Some(Land) => Set.Int.empty
  | Some(None) => Set.Int.empty
  | Some(Army(_, Bomb)) => Set.Int.empty
  | Some(Army(_, Flag)) => Set.Int.empty
  | Some(Army(side, kind)) => {
      let rec step = (rs, indexes) =>
        switch indexes {
        | list{} => rs
        | list{hd, ...tl} =>
          switch board[hd] {
          | None => rs
          | Some(tile) =>
            switch walk(Army(side, kind), tile) {
            | (true, true) => step(Set.Int.add(rs, hd), tl)
            | (true, false) => Set.Int.add(rs, hd)
            | (false, _) => rs
            }
          }
        }

      let rec hike = (rs, dirs) =>
        switch dirs {
        | list{} => rs
        | list{hd, ...tl} => hike(step(rs, path(index, kind, hd)), tl)
        }

      hike(Set.Int.empty, list{N, E, S, W})
    }
  }
}

let opposing = s =>
  switch s {
  | Red => Blue
  | Blue => Red
  }

let starting = ks => {
  let rec recur = (xs, ks) => {
    switch ks {
    | list{} => xs
    | list{hd, ...tl} => recur(conj(xs, repeat(hd, quantity(hd))), tl)
    }
  }

  recur(list{}, ks)
}

let terrain = Array.concatMany([
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
])

let kinds = list{Bomb, Mars, Genr, Crnl, Majr, Capt, Lieu, Serg, Mine, Scot, Spys, Flag}

type event =
  | StartNew
  | Arrange(kind, (int, int))
  | Rearrange((int, int), (int, int))

type state =
  | Invalid
  | NotStarted
  | Setup({tiles: array<tile>, side: side, yours: list<kind>, theirs: list<kind>})

let resolve = (state, event) =>
  switch (state, event) {
  | (NotStarted, StartNew) =>
    Setup({tiles: terrain, side: Red, yours: starting(kinds), theirs: starting(kinds)})

  | (Setup({tiles, side, yours} as record), Arrange(kind, coord)) => {
      let pos = index(coord)
      switch tiles[pos] {
      | None => state
      | Some(None) => state
      | Some(Army(s, _)) when s === opposing(side) => state
      | Some(tile) =>
        if validStart(side, coord) {
          let remaining = switch tile {
          | Army(_, replaced) => list{replaced, ...remove(yours, kind)}
          | _ => remove(yours, kind)
          }

          let board = assoc(tiles, pos, Army(side, kind))
          Setup({...record, tiles: board, yours: remaining})
        } else {
          state
        }
      }
    }

  | (Setup({tiles, side} as record), Rearrange(a, b)) => {
      let pos = index(a)
      switch tiles[pos] {
      | None => state
      | Some(None) => state
      | Some(Army(s, _)) when s === opposing(side) => state
      | Some(Army(_, _)) => {
          let h = index(a)
          let j = index(b)
          let t = switch (tiles[h], tiles[j]) {
          | (Some(at), Some(bt)) => tiles->assoc(h, bt)->assoc(j, at)
          | _ => tiles
          }

          Setup({...record, tiles: t})
        }
      | _ => state
      }
    }

  | _ => state
  }
