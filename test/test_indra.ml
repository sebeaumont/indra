open Indra.Math
module CC = ComplexExt
module M = Mobius

(* the most basic tests ever *)
let () =
  let z = { CC.re = 1.714; im = -1.234 } in
  let w = { CC.re = -1.714; im = 3.142 } in
  let v = { CC.re = 0.3010; im = 3.142 } in
  let u = { CC.re = -31.42; im = 17.34 } in
  let m1 = M.matrix z w v u in

  (* basic exteded arithmetic *)
  Format.printf "%s%!" "Divide z by zero...";
  assert (CC.divx z CC.zero = CC.infinity);
  assert (CC.divx w CC.zero = CC.infinity);
  assert (CC.divx v CC.zero = CC.infinity);
  assert (CC.divx u CC.zero = CC.infinity);
  assert (CC.is_nan (CC.divx CC.zero CC.zero));
  assert (CC.is_nan (CC.divx CC.infinity CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" "Adding infinity...";
  assert (CC.addx z CC.infinity = CC.infinity);
  assert (CC.addx CC.infinity z = CC.infinity);
  assert (CC.is_nan (CC.addx CC.infinity CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" "Subtracting infinity...";
  assert (CC.subx z CC.infinity = CC.infinity);
  assert (CC.subx CC.infinity z = CC.infinity);
  assert (CC.is_nan (CC.subx CC.infinity CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" "Multiply z by infinity...";
  assert (CC.mulx z CC.infinity = CC.infinity);
  assert (CC.mulx w CC.infinity = CC.infinity);
  assert (CC.mulx v CC.infinity = CC.infinity);
  assert (CC.mulx u CC.infinity = CC.infinity);
  assert (CC.mulx u CC.zero = CC.zero);
  assert (CC.mulx CC.zero z = CC.zero);
  assert (CC.is_nan (CC.mulx CC.infinity CC.zero));
  assert (CC.is_nan (CC.mulx CC.zero CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" "Mobius invariants...";
  assert (M.is_normal m1);
  assert (M.transform_point m1 CC.infinity = CC.divx m1.a m1.c);
  assert (M.compose m1 M.identity = m1);
  assert (M.compose M.identity m1 = m1);
  Format.printf "should this be infinity? %a\n" CC.pp
    (M.transform_point m1 (CC.neg (CC.divx m1.c m1.d)));

  Format.printf "OK\n"
