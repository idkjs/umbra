let state: Type.state = {test: "Hello, world!"}
let dispatch = (_: Type.event) => ()
let context = React.createContext((state, dispatch))
let use = _ => React.useContext(context)

module Provider = {
  let make = React.Context.provider(context)
  let makeProps = (~value, ~children, ()) => {"value": value, "children": children}
}
