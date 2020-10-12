type terrain =
  | None
  | Land

type status =
  | Camped
  | Marshalled(Vector.t)
  | Captured

type t =
  | Field({terrain: terrain, vector: Vector.t})
  | Corps({rank: Rank.t, fealty: Fealty.t, status: status})

let opposes = (atk, def) =>
  switch (atk, def) {
  | (Corps({fealty: h}), Corps({fealty: k})) => h !== k
  | _ => false
  }

let walk = (atk, def) =>
  switch (atk, def) {
  | (Corps(_), Corps(_)) => (opposes(atk, def), false)
  | (Corps(_), Field(_)) => (true, true)
  | (_, _) => (false, false)
  }
