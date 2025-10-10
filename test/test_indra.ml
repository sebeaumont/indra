open Indra.Math
module CC = ComplexExt
module M = Mobius

let pad ?(n = 40) ?(c = '.') s =
  let slen = String.length s in
  if slen < n then s ^ String.make (n - slen) c else s

(* basic tests *)

let () =
  let r2 = Float.sqrt 2.0 in

  (* nonsese *)
  let z = { CC.re = 1.714; im = -1.234 } in
  let w = { CC.re = -1.714; im = 3.142 } in
  let v = { CC.re = 0.3010; im = 3.142 } in
  let u = { CC.re = -31.42; im = 17.34 } in

  (* elliptic *)
  let ea = { CC.re = r2 /. 2.; im = 0. } in
  let eb = { CC.re = r2 /. -2.; im = 0. } in
  let ec = { CC.re = r2 /. 2.; im = 0. } in
  let ed = { CC.re = r2 /. 2.; im = 0. } in

  (* hyperbolic *)
  let ha = { CC.re = 2.; im = 0. } in
  let hb = { CC.re = 0.; im = 0. } in
  let hc = { CC.re = 0.; im = 0. } in
  let hd = { CC.re = 1.; im = 0. } in

  (* parabolic *)
  let pa = { CC.re = 1.; im = 0. } in
  let pb = { CC.re = 1.; im = 0. } in
  let pc = { CC.re = 0.; im = 0. } in
  let pd = { CC.re = 1.; im = 0. } in

  let m1 = M.matrix z w v u in
  let me = M.matrix ea eb ec ed in
  let mh = M.matrix ha hb hc hd in
  let _mp = M.matrix pa pb pc pd in

  (* basic exteded arithmetic *)
  Format.printf "%s%!" (pad "Divide z by zero");
  assert (CC.divx z CC.zero = CC.infinity);
  assert (CC.divx w CC.zero = CC.infinity);
  assert (CC.divx v CC.zero = CC.infinity);
  assert (CC.divx u CC.zero = CC.infinity);
  assert (CC.is_nan (CC.divx CC.zero CC.zero));
  assert (CC.is_nan (CC.divx CC.infinity CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" (pad "Adding infinity");
  assert (CC.addx z CC.infinity = CC.infinity);
  assert (CC.addx CC.infinity z = CC.infinity);
  assert (CC.is_nan (CC.addx CC.infinity CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" (pad "Subtracting infinity");
  assert (CC.subx z CC.infinity = CC.infinity);
  assert (CC.subx CC.infinity z = CC.infinity);
  assert (CC.is_nan (CC.subx CC.infinity CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" (pad "Multiply z by infinity");
  assert (CC.mulx z CC.infinity = CC.infinity);
  assert (CC.mulx w CC.infinity = CC.infinity);
  assert (CC.mulx v CC.infinity = CC.infinity);
  assert (CC.mulx u CC.infinity = CC.infinity);
  assert (CC.mulx u CC.zero = CC.zero);
  assert (CC.mulx CC.zero z = CC.zero);
  assert (CC.is_nan (CC.mulx CC.infinity CC.zero));
  assert (CC.is_nan (CC.mulx CC.zero CC.infinity));
  Format.printf "OK\n";

  Format.printf "%s%!" (pad "Mobius invariants");
  assert (M.is_normal m1);
  assert (M.transform_point m1 CC.infinity = CC.divx m1.a m1.c);
  assert (M.compose m1 M.identity = m1);
  assert (M.compose M.identity m1 = m1);
  assert (M.transform_point m1 (CC.neg (CC.divx m1.d m1.c)) = CC.infinity);
  Format.printf "OK\n";

  Format.printf "%s%!" (pad "Mobius mangling");
  Format.printf "%s\n" (M.to_string me);

  let foo =
    let zed = ref z in
    (* some iterations *)
    for _ = 0 to 20 do
      zed := M.transform_point me !zed;
      Format.printf "%s\n" (CC.to_string !zed)
    done in
  foo;

  Format.printf "\n";

  let bar =
    let t = ref mh in
    (* some iterations *)
    for _ = 0 to 20 do
      t := M.compose !t !t;
      Format.printf "%s\n" (M.to_string !t)
    done in
  bar
