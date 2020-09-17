open Belt

let css = ts => Array.reduce(ts, "", (names, (name, pass)) =>
    switch (pass, names) {
    | (true, "") => name
    | (true, names) => Js.String.concatMany([" ", name], names)
    | (false, names) => names
    }
  )

let tileName = (t: State.tile) =>
  switch t {
  | None => "~"
  | Land => ""
  | Army(_, k) =>
    switch k {
    | Bomb => "Bomb"
    | Mars => "Mars"
    | Genr => "Genr"
    | Crnl => "Crnl"
    | Majr => "Majr"
    | Capt => "Capt"
    | Lieu => "Lieu"
    | Serg => "Serg"
    | Mine => "Mine"
    | Scot => "Scot"
    | Spys => "Spys"
    | Flag => "Flag"
    }
  }

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

module Home = {
  @react.component
  let make = () => {
    let (selected, setSelected) = React.useState(_ => None)
    let (state, dispatch) = Context.use()

    switch state {
    | Invalid => <div> {React.string("Invalid state")} </div>

    | NotStarted =>
      <div className="controls">
        <button onClick={_ => dispatch(StartNew)}> {React.string("Start new game")} </button>
      </div>

    | Setup({tiles, side, yours}) => {
        let board = Array.mapWithIndex(tiles, (index, tile) => {
          let coords = State.coords(index)
          let allowed = State.validStart(side, coords)
          <div
            className={css([("allowed", selected !== None && allowed)])}
            onClick={_ =>
              switch (allowed, selected) {
              | (false, _) => ()
              | (true, None) => ()
              | (true, Some(kind)) =>
                setSelected(_ => None)
                dispatch(Set(kind, coords))
              }}>
            {React.string(tileName(tile))}
          </div>
        })

        let pieces =
          yours
          ->List.toArray
          ->Array.map(kind =>
            <div onClick={_ => setSelected(_ => Some(kind))}>
              {React.string(tileName(Army(side, kind)))}
            </div>
          )

        <div>
          <div className="board"> {React.array(board)} </div>
          <div className="pieces"> {React.array(pieces)} </div>
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
