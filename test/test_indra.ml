open Indra.Math
module CC = ComplexExt

(* contorted test as nan's are not comparable as numbers *)
let is_nan (z : CC.t) =
  string_of_float z.re = "nan" && string_of_float z.re = "nan"

(* the most basic tests ever *)
let () =
  let z = { CC.re = 1.714; im = -1.234 } in
  let w = { CC.re = -1.714; im = 3.142 } in
  let v = { CC.re = 0.3010; im = 3.142 } in
  let u = { CC.re = -31.42; im = 17.34 } in
  (* basic exteded arithmetic *)
  Printf.printf "%s%!" "Divide z by zero...";
  assert (CC.divx z CC.zero = CC.infinity);
  assert (CC.divx w CC.zero = CC.infinity);
  assert (CC.divx v CC.zero = CC.infinity);
  assert (CC.divx u CC.zero = CC.infinity);
  assert (is_nan (CC.divx CC.zero CC.zero));
  assert (is_nan (CC.divx CC.infinity CC.infinity));
  Printf.printf "OK\n";

  Printf.printf "%s%!" "Adding infinity...";
  assert (CC.addx z CC.infinity = CC.infinity);
  assert (CC.addx CC.infinity z = CC.infinity);
  assert (is_nan (CC.addx CC.infinity CC.infinity));
  Printf.printf "OK\n";

  Printf.printf "%s%!" "Subtracting infinity...";
  assert (CC.subx z CC.infinity = CC.infinity);
  assert (CC.subx CC.infinity z = CC.infinity);
  assert (is_nan (CC.subx CC.infinity CC.infinity));
  Printf.printf "OK\n";

  Printf.printf "%s%!" "Multiply z by infinity...";
  assert (CC.mulx z CC.infinity = CC.infinity);
  assert (CC.mulx w CC.infinity = CC.infinity);
  assert (CC.mulx v CC.infinity = CC.infinity);
  assert (CC.mulx u CC.infinity = CC.infinity);
  assert (CC.mulx u CC.zero = CC.zero);
  assert (CC.mulx CC.zero z = CC.zero);
  assert (is_nan (CC.mulx CC.infinity CC.zero));
  assert (is_nan (CC.mulx CC.zero CC.infinity));
  Printf.printf "OK\n"
