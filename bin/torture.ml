open Indra

let () =
  let g = Group.make [ ('b', Mobius.identity); ('a', Mobius.identity) ] in
  Group.dfs_paths g 15
