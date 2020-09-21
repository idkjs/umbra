open Belt

let css = ts => Array.reduce(ts, "", (names, (name, pass)) =>
    switch (pass, names) {
    | (true, "") => name
    | (true, names) => Js.String.concatMany([" ", name], names)
    | (false, names) => names
    }
  )

let navigate = (path, _) => ReasonReactRouter.push("#" ++ path)

module Link = {
  @react.component
  let make = (~children, ~path) => {
    <a href="#" onClick={navigate(path)}> {children} </a>
  }
}

module Layout = {
  @react.component
  let make = (~children) => {
    <main>
      <h1 className="title"> <Link path="/"> {React.string("Umbra")} </Link> </h1> {children}
    </main>
  }
}

module Piece = {
  @react.component
  let make = (~player: Fealty.t, ~tile: Tile.t, ~onSelect=_ => (), ~hinting=false) => {
    let classNames = switch tile {
    | Field(_) => [("tile", true)]
    | Corps(_) => [("tile", true)]
    }

    <div className={css(classNames)} onClick={_ => onSelect(tile)}>
      {React.string(
        switch tile {
        | Field({terrain: None}) => "~"
        | Field({terrain: Land}) => ""
        | Corps({rank}) => string_of_int(Rank.strength(rank))
        },
      )}
    </div>
  }
}

module Home = {
  type context =
    | Camp
    | Board((int, int))

  @react.component
  let make = () => {
    let (state, dispatch) = Context.use()
    let (selected: option<Tile.t>, setSelected) = React.useState(_ => None)

    let onSelect = (selecting: Tile.t) =>
      switch (selected, selecting) {
      | (None, Corps(_)) => setSelected(_ => Some(selecting))
      | (None, _) => ()
      | (Some(selected), _) =>
        switch (selected, selecting) {
        | (Corps({rank, status: Camped}), Field({vector: dest}))
        | (Corps({rank, status: Camped}), Corps({status: Marshalled(dest)})) =>
          dispatch(Arrange(rank, dest))

        | (Corps({status: Marshalled(src)}), Field({vector: dst}))
        | (Corps({status: Marshalled(src)}), Corps({status: Marshalled(dst)})) =>
          dispatch(Rearrange(src, dst))

        | _ => ()
        }

        setSelected(_ => None)
      }

    switch state {
    | Invalid => <div> {React.string("Invalid state")} </div>

    | NotStarted =>
      <div className="controls">
        <button onClick={_ => dispatch(StartNew)}> {React.string("Start new game")} </button>
      </div>

    | Setup({board, fealty: player, yours, theirs}) => {
        let tiles = board->Array.map(tile => <Piece player tile onSelect />)

        let friendly = yours->List.toArray->Array.map(rank => {
          let tile = Tile.Corps({rank: rank, fealty: player, status: Camped})
          <Piece player tile onSelect />
        })

        let opponent = theirs->List.toArray->Array.map(rank => {
          let tile = Tile.Corps({rank: rank, fealty: player, status: Camped})
          <Piece player tile onSelect />
        })

        <div className="layout">
          <div className="pieces"> {React.array(player == Fealty.Blue ? friendly : opponent)} </div>
          <div className="board"> {React.array(tiles)} </div>
          <div className="pieces"> {React.array(player == Fealty.Red ? friendly : opponent)} </div>
        </div>
      }
    }
  }
}

module NotFound = {
  @react.component
  let make = () => {
    <div> {React.string("Page not found.")} </div>
  }
}

module Router = {
  @react.component
  let make = () => {
    let url = ReasonReactRouter.useUrl()
    switch Js.String.split("/", url.hash) {
    | [""] => <Home />
    | _ => <NotFound />
    }
  }
}

module Root = {
  let thunk = (dispatch, event: State.event) => {
    switch event {
    | event => dispatch(event)
    }
  }

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(State.resolve, State.NotStarted)
    <Context.Provider value={(state, thunk(dispatch))}>
      <Layout> <Router /> </Layout>
    </Context.Provider>
  }
}
