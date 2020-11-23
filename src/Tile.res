type terrain =
  | None
  | Land

type status =
  | Camped
  | Marshalled
  | Captured

type t =
  | Field({vector: Vector.t, terrain: terrain})
  | Corps({vector: Vector.t, rank: Rank.t, fealty: Fealty.t, status: status})

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
