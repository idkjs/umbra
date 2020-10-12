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
  let make = (~children, ~path, ~className="") => {
    <a href="#" className={className} onClick={navigate(path)}> {children} </a>
  }
}

module Piece = {
  @react.component
  let make = (~tile: Tile.t) => {
    let (state, dispatch) = Context.use()

    let classes = ["tile", "corps", "red", "blue", "hint", "hidden", "friendly"]
    let classNames = Array.map(classes, name => {
      switch (name, tile) {
      | ("tile", _) => (name, true)
      | ("corps", Corps(_)) => (name, true)
      | ("red", Field({vector})) => (name, Board.inBounds(Red, vector))
      | ("red", Corps({fealty: Red})) => (name, true)
      | ("blue", Field({vector})) => (name, Board.inBounds(Blue, vector))
      | ("blue", Corps({fealty: Blue})) => (name, true)
      | ("hint", _) => (
          name,
          switch (state, tile) {
          | (Setup({fealty: h}), Corps({fealty: k})) when h !== k => false
          | (Setup({selected: None}), Corps(_)) => true
          | (Setup({fealty, selected: Some(_)}), Field({vector})) => Board.inBounds(fealty, vector)
          | (
              Setup({selected: Some(Corps({status: Marshalled(a)}))}),
              Corps({status: Marshalled(b)}),
            ) =>
            !Vector.eq(a, b)
          | (Setup({selected: Some(_)}), Corps({status: Marshalled(_)})) => true
          | _ => false
          },
        )
      | _ => (name, false)
      }
    })

    <div className={css(classNames)} onClick={_ => dispatch(Select(tile))}>
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

module Control = {
  @react.component
  let make = (~children, ~event: State.event, ~enabled: State.t => bool) => {
    let (state, dispatch) = Context.use()
    <button disabled={!enabled(state)} onClick={_ => dispatch(event)}> {children} </button>
  }
}

module Panel = {
  @react.component
  let make = () => {
    let (state, dispatch) = Context.use()
    <>
      <header>
        <h1 className="title"> {React.string("umbra")} </h1>
        <p> {React.string("the classic game of battlefield strategy")} </p>
      </header>
      <section className="ns-section">
        <h2> {React.string("Start")} </h2>
        <Control
          event={StartNew(Blue)}
          enabled={state =>
            switch state {
            | NotStarted => true
            | _ => false
            }}>
          {React.string("Blue")}
        </Control>
        <Control
          event={StartNew(Red)}
          enabled={state =>
            switch state {
            | NotStarted => true
            | _ => false
            }}>
          {React.string("Red")}
        </Control>
      </section>
      <section className="ns-section">
        <h2> {React.string("Setup")} </h2>
        <Control
          event={Shuffle}
          enabled={state =>
            switch state {
            | Setup({yours}) => List.some(yours, _ => true)
            | _ => false
            }}>
          {React.string("Shuffle")}
        </Control>
        <Control
          event={Reset}
          enabled={state =>
            switch state {
            | Setup(_) => true
            | _ => false
            }}>
          {React.string("Reset")}
        </Control>
        <Control
          event={Start}
          enabled={state =>
            switch state {
            | Setup({yours}) => !List.some(yours, _ => true)
            | _ => false
            }}>
          {React.string("Begin")}
        </Control>
        <Control
          event={Quit}
          enabled={state =>
            switch state {
            | Setup(_) => true
            | _ => false
            }}>
          {React.string("Quit")}
        </Control>
      </section>
      <section className="ns-section">
        <h2> {React.string("Play")} </h2>
        <Control
          event={Shuffle}
          enabled={state =>
            switch state {
            | Playing(_) => true
            | _ => false
            }}>
          {React.string("Forfeit")}
        </Control>
        <Control
          event={Shuffle}
          enabled={state =>
            switch state {
            | Playing(_) => true
            | _ => false
            }}>
          {React.string("Skip")}
        </Control>
      </section>
    </>
  }
}

module Board = {
  @react.component
  let make = () => {
    let (state, dispatch) = Context.use()

    switch state {
    | Setup({board, fealty: player, yours, theirs}) =>
      let tiles = board->Array.map(tile => <Piece tile />)

      let friendly = yours->List.toArray->Array.map(rank => {
        let tile = Tile.Corps({rank: rank, fealty: player, status: Camped})
        <Piece tile />
      })

      let opponent = theirs->List.toArray->Array.map(rank => {
        let tile = Tile.Corps({rank: rank, fealty: Fealty.opposing(player), status: Camped})
        <Piece tile />
      })

      <div>
        <div className="pieces"> {React.array(player == Fealty.Blue ? friendly : opponent)} </div>
        <div className="board"> {React.array(tiles)} </div>
        <div className="pieces"> {React.array(player == Fealty.Red ? friendly : opponent)} </div>
      </div>

    | _ => <div />
    }
  }
}

module Layout = {
  @react.component
  let make = () => {
    <div className="ns-layout">
      <div className="ns-layout-panel"> <Panel /> </div>
      <div className="ns-layout-board"> <Board /> </div>
    </div>
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
    <Context.Provider value={(state, thunk(dispatch))}> <Layout /> </Context.Provider>
  }
}
