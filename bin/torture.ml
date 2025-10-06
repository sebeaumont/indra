open Indra.Math

let () =
  let g = { Group.generators = "aBAb" } in
  Group.dfs g 15
