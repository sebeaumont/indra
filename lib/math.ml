(* Math for Indra's pearls and all things to be found in the complex plane
   Copyleft 2025 Simon Beaumont, Newport, England *)

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

  (* nota bene a Float.nan cannot be compared as number so... *)
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

  (* smart constructor: check for singularities and normalize so det = 1
     so these then can be elements of the PSL(2,C) or Kleinian group *)

  let transformation a b c d =
    let det = sub (mul a d) (mul b c) in
    if det = zero then raise Singular
    else
      let uniter = sqrt det in
      { a = div a uniter; b = div b uniter; c = div c uniter; d = div d uniter }

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
  let transform_point m z = div (add (mul m.a z) m.b) (add (mul m.c z) m.d)
end

(* -------------------------------------------------------------------------- *)

module Geometry = struct
  (* like the circles of your mind... *)
  type circle = { centre : ComplexExt.t; r : Float.t }
end

(* -------------------------------------------------------------------------- *)
