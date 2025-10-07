open Indra.Math

let () =
  let g = Group.make [ ('b', Mobius.identity); ('a', Mobius.j) ] in
  Group.dfs_paths g 15
