@bs.val
external addEventListener: (string, unit => unit) => unit = "window.addEventListener"

let main = _ => {
  switch ReactDOM.querySelector("#umbra") {
  | Some(element) => ReactDOM.render(<Component.Root />, element)
  | None => ()
  }
}

addEventListener("DOMContentLoaded", main)
