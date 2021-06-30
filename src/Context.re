let dispatch = (_: State.event) => ();
let context = React.createContext((State.NotStarted, dispatch));
let use = _ => React.useContext(context);

module Provider = {
  let make = React.Context.provider(context);
  let makeProps = (~value, ~children, ()) => {
    "value": value,
    "children": children,
  };
};
