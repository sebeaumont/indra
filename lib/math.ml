(* Math for Indra's pearls and all things to be found in the complex plane
   Copyleft 2025 Simon Beaumont, Newport, England *)

(* -------------------------------------------------------------------------- *)

module ComplexExt = struct
  (** Complex numbers extended. We extend the complex numbers to include the
      point at infinity and provide extended arithmetic. *)

  (* using the following
      rules:

      | a + ∞ = ∞ 
      | a - ∞ = ∞ 
      | a * ∞ = ∞ if a <> 0
      | ∞ / a = ∞
      | a / 0 = ∞ if a <> 0

      | ∞ +- ∞ = nan
      | 0 * ∞ = nan
      | 0 / 0 = nan
      | ∞ / ∞ = nan

      Complex.infinity has infinite magnitude and undefined phase. *)

  include Complex

  (* nota bene a Float.nan cannot be compared as a number *)
  let infinity = { re = Float.infinity; im = Float.zero }
  let nan = { re = Float.nan; im = Float.nan }

  (* extended operations *)
  let divx a b =
    if a <> zero then if b <> zero then div a b else infinity
    else if b = zero then nan
    else zero

  let addx a b =
    if a = infinity && b = infinity then nan
    else if a = infinity || b = infinity then infinity
    else add a b

  let subx a b =
    if a = infinity && b = infinity then nan
    else if a = infinity || b = infinity then infinity
    else sub a b

  let mulx a b =
    if a <> zero then if b = infinity then infinity else mul a b
    else if b = infinity then nan
    else mul a b
end

(* -------------------------------------------------------------------------- *)

module Mobius = struct
  (** Mobius transformations using the extended complex plane *)

  (* should we just use the extended arithmetic everywhere? *)
  open ComplexExt

  (* representation of a Mobius tranformation *)

  type t = {
    a : ComplexExt.t;
    b : ComplexExt.t;
    c : ComplexExt.t;
    d : ComplexExt.t;
  }

  let determinant m = sub (mul m.a m.d) (mul m.b m.c)

  exception Singular

  (* convenience constructor apply normalisation? *)
  let matrix a b c d = { a; b; c; d }

  (* check for singularity and normalise so det = 1 and an element of
     the PSL(2,C) Kleinian group *)

  let normalise m =
    let det = sub (mul m.a m.d) (mul m.b m.c) in
    if det = zero then raise Singular
    else
      (* could be -ve or +ve and hence special *)
      let uniter = sqrt det in
      {
        a = div m.a uniter;
        b = div m.b uniter;
        c = div m.c uniter;
        d = div m.d uniter;
      }

  (* composition is "matrix" multiplication *)

  let compose m1 m2 =
    {
      a = add (mul m1.a m2.a) (mul m1.b m2.c);
      b = add (mul m1.a m2.b) (mul m1.b m2.d);
      c = add (mul m1.c m2.a) (mul m1.d m2.c);
      d = add (mul m1.c m2.b) (mul m1.d m2.d);
    }

  let trace m = add m.a m.d
  let inverse m = { m with a = neg m.d; d = neg m.a }

  (* need another look at this w.r.t extended arithmetic  *)
  let transform_point m z = div (add (mul m.a z) m.b) (add (mul m.c z) m.d)
  let identity = matrix one zero zero one
  let one_over_z = matrix zero one one zero

  (* sign is irrelevant in special group *)
  let is_normal m = norm2 (determinant m) = Float.one

  (*  general fixed point formula - can simplify if assumed normal *)

  let fixed_points m =
    let four = { re = 4.; im = Float.zero } in
    let dma = sub m.d m.a in
    let dma2 = mul dma dma in
    let bc4 = mul four (mul m.b m.c) in
    let discr = sqrt (add dma2 bc4) in
    let a_d = sub m.a m.d in
    let two = { re = 2.; im = Float.zero } in
    let two_c = mul two m.c in
    let r1 = divx (sub a_d discr) two_c in
    let r2 = divx (add a_d discr) two_c in
    (r1, r2)
end

(* -------------------------------------------------------------------------- *)

module Geometry = struct
  (* like the circles of your mind... *)
  type circle = { centre : ComplexExt.t; r : Float.t }
end

(* -------------------------------------------------------------------------- *)
