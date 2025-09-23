open Indra.Math
open Complex

let () =
  let z = {re = -4.17234; im = 3.142} in
  let w = {re = 2.1723; im = -2.125} in
  let mt = Mobius.transformation z z w z in
  Printf.printf "%f" mt.d.im


