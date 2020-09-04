module Umbra = {
  open Belt

  type direction =
    | N
    | E
    | S
    | W

  type side =
    | Blue
    | Red

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
    | Army(side, kind)
    | Land
    | None

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
    | (Mine, Bomb) => true
    | (Spys, Mars) => true
    | _ => rank(a) > rank(b)
    }

  let kinds = [Bomb, Mars, Genr, Crnl, Majr, Capt, Lieu, Serg, Mine, Scot, Spys, Flag]
  let flatMap = (xs, f) => Array.reduce(xs, [], (r, x) => Array.concat(r, f(x)))
  let newSet = s => flatMap(kinds, k => Array.make(quantity(k), Army(s, k)))
  let coords = i => (mod(i, 10), i / 10)
  let index = ((x, y)) => y * 10 + x

  let newBoard = _ => {
    let lts = [Land, Land, None, None, Land, Land, None, None, Land, Land]
    let bts = Array.shuffle(newSet(Blue))
    let rts = Array.shuffle(newSet(Red))
    Array.concatMany([bts, lts, lts, rts])
  }

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

  let walk = (a, b) =>
    switch (a, b) {
    | (Army(h, _), Army(k, _)) => (h !== k, false)
    | (Army(_, _), Land) => (true, true)
    | (_, _) => (false, false)
    }

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
}
