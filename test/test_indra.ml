open Indra
module CC = Complex
module M = Mobius
(* module D = Data *)

let pad ?(n = 40) ?(c = '.') s =
  let slen = String.length s in
  if slen < n then s ^ String.make (n - slen) c else s

let safe_points =
  [|
    { CC.re = 0.1; im = 0.2 };
    { CC.re = 0.3; im = 0.4 };
    { CC.re = 0.5; im = 0.6 };
    { CC.re = 0.7; im = 0.8 };
  |]

let edge_points =
  [|
    { CC.re = 1e-10; im = 0. };
    { CC.re = 1e10; im = 0. };
    { CC.re = 0.; im = 1. };
    { CC.re = 1.; im = 0. };
  |]

(* Test if composition preserves cross-ratio (the ONLY stable test) *)
let test_mobius_stability t1 t2 zs =
  (* Compute cross-ratio BEFORE composition *)
  let cr_original = M.from_array zs |> M.cross_ratio in

  (* Compose transformations stably (using QR) *)
  let t_composed = M.compose t1 t2 in

  (* Apply composed transformation to all points *)
  let z_transformed =
    Array.map (M.transform_point t_composed) zs |> M.from_array in
  let cr_transformed = M.cross_ratio z_transformed in
  (* Check invariance (floating-point safe!) *)
  Complex.norm (Complex.sub cr_original cr_transformed) < 1e-10

(* basic tests *)

let () =
  let r2 = Float.sqrt 2.0 in

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

  let me = M.matrix ea eb ec ed in
  let mh = M.matrix ha hb hc hd in
  let mp = M.matrix pa pb pc pd in

  let z = { CC.re = 0.3; im = 0.4 } in

  Format.printf "%s%!" (pad "Mobius invariants");
  assert (M.compose M.identity me = me);
  Format.printf "OK@.";

  Format.printf "%s%!" (pad "Mobius stability");
  assert (test_mobius_stability me mh safe_points);
  assert (test_mobius_stability mh mp safe_points);

  assert (test_mobius_stability me mh edge_points);
  assert (test_mobius_stability mh mp edge_points);
  Format.printf "OK@.";

  Format.printf "%s@." (pad "Mobius mangling");

  let pp_complex ppf (z : Complex.t) =
    let open Complex in
    let sign = if z.im >= 0.0 then "+" else "" in
    Format.fprintf ppf "(%f%s%fi)" z.re sign z.im in

  let foo =
    let zed = ref z in
    (* some iterations *)
    for i = 0 to 20 do
      Format.printf "%d,%!" i;
      zed := M.transform_point me !zed;
      Format.printf "%a\n" pp_complex !zed
    done in
  foo;

  Format.printf "@.";

  (* this is blowing up to infinity and beyond! *)
  let bar =
    let t = ref me in
    (* some iterations *)
    for i = 0 to 20 do
      Format.printf "%d,%!" i;
      t := M.compose me !t;
      Format.printf "%a@." M.pp !t
    done in
  bar
