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
    let (state, dispatch) = Context.use()
    dispatch(Foo)
    Js.log(state)
    <div className="controls"> <button> {React.string("Start new game")} </button> </div>
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
  let reducer = (state, event) => {
    switch (event: Type.event) {
    | Foo => state
    | Bar => state
    }
  }

  let thunk = (dispatch, event: Type.event) => {
    switch event {
    | event => dispatch(event)
    }
  }

  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, ({test: "Hello, world!"}: Type.state))
    <Context.Provider value={(state, thunk(dispatch))}>
      <Layout> <Router /> </Layout>
    </Context.Provider>
  }
}
