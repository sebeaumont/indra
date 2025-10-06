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

      Complex.infinity has infinite magnitude and undefined phase.
   *)

  include Complex

  let pp ppf { re; im } =
    let sign = if im >= 0.0 then "+" else "" in
    Format.fprintf ppf "(%f%s%fi)" re sign im

  (* this is how we define ComplexExt.nan *)
  let is_nan (z : Complex.t) = Float.is_nan z.re && Float.is_nan z.re

  (* nota bene a Float.nan cannot be compared as a number *)
  let infinity = { re = Float.infinity; im = Float.zero }
  let nan = { re = Float.nan; im = Float.nan }
  let f2c f = { re = f; im = Float.zero }

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

module Geometry = struct
  (* like the circles of your mind... *)
  type circle = { centre : ComplexExt.t; r : Float.t }
end

(* -------------------------------------------------------------------------- *)

module Mobius = struct
  (** Mobius transformations using the extended complex plane *)

  (* should we just use the extended arithmetic everywhere? *)
  open ComplexExt
  open Geometry

  (* representation of a Mobius tranformation *)

  type t = {
    a : ComplexExt.t;
    b : ComplexExt.t;
    c : ComplexExt.t;
    d : ComplexExt.t;
  }

  (* determinant of 2x2 complex matrix *)
  let determinant m = sub (mul m.a m.d) (mul m.b m.c)

  exception Singular

  (* convenience constructor apply normalisation? *)
  let matrix a b c d = { a; b; c; d }

  (* check for singularity and normalise so det = 1 and an acceptable
     element of the PSL(2,C) or Kleinian group *)
  let normalise m =
    let det = determinant m in
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

  (* assumed to have  det 1 from here on *)
  let map a b c d = normalise @@ matrix a b c d

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

  (* transform a point in complex plane *)
  let transform_point m z =
    if z = infinity then divx m.a m.c
    else divx (add (mul m.a z) m.b) (add (mul m.c z) m.d)

  (* transform a circle in complex plane *)
  let transform_circle m c =
    let w = conj (add c.centre (divx m.d m.c)) in
    let r2 = f2c (c.r *. c.r) in
    let z = subx c.centre (divx r2 w) in
    let centre = transform_point m z in
    let radius =
      norm (sub centre (transform_point m (add c.centre (f2c c.r)))) in
    { centre; r = radius }

  (* useful transformations *)
  let identity = matrix one zero zero one
  let j = matrix zero i i zero

  (* floating point equality - since we are normalised then this should
     be ok *)
  let almost_equal a b =
    let d = Float.abs (a -. b) in
    let limit = 2.0 *. Float.epsilon in
    d <= limit

  (* sign is irrelevant in special group *)
  let is_normal m = almost_equal (norm2 (determinant m)) Float.one

  (*  general fixed point formula - can simplify if assumed normal *)
  let fixed_points m =
    let four = f2c 4.0 in
    let dma = sub m.d m.a in
    let dma2 = mul dma dma in
    let bc4 = mul four (mul m.b m.c) in
    let discr = sqrt (add dma2 bc4) in
    let a_d = sub m.a m.d in
    let two = f2c 2.0 in
    let two_c = mul two m.c in
    let r1 = divx (sub a_d discr) two_c in
    let r2 = divx (add a_d discr) two_c in
    (r1, r2)
end

(* -------------------------------------------------------------------------------- *)

module Group = struct
  type t = { generators : String.t }

  (** Nota Bene: We rely on a certain ordering of generator letters for
      efficient adjacent inverse avoidance in reduced form: e.g. abAB avoids
      such if we only look to our immediate neighbours and ourselves as valid
      next letters (cyclically), abcABC etc. However to ensure nice generation
      behaviour on depth first traversal of the "tree" we cyclically permute the
      letters anti-clockwise. Whatever nonsense string you give us here this
      should sort it out. *)

  let make ~letters =
    let s = String.lowercase_ascii letters in
    (* sort uniq in dictionary l/c and append the inverses u/c *)
    let l =
      String.to_seq s
      |> List.of_seq
      |> List.sort_uniq Char.compare
      |> List.to_seq
      |> String.of_seq in
    let gens = String.cat l (String.uppercase_ascii l) in
    (* do an anti-clockwise cyclic permuatation so dfs works nicely *)
    let cycle_ac x =
      let len = String.length x in
      String.init len (function
        | 0 -> String.get x 0
        | n -> String.get x (len - n)) in
    { generators = cycle_ac gens }

  let glen g = String.length g.generators
  let gcla g i = if i < 0 then glen g + i else i mod glen g
  let goff g = (glen g / 2) - 1 (* glen is always even *)

  (* keep track of paths for testing *)
  let dfs_paths g n =
    let rec explore l d p =
      if d < n then (
        Printf.printf "%s --> " p;
        (* traverse successors *)
        for i = l - goff g to l + goff g do
          let k = gcla g i in
          explore k (d + 1) (* depth first *)
            (String.cat p (String.get g.generators k |> String.make 1))
        done) in
    (* root level *)
    for r = 0 to glen g - 1 do
      explore r 0 (String.get g.generators r |> String.make 1);
      Printf.printf "\n"
    done

  (* lookup transformation for group letter index *)

  (* fold depth first over group generators stopping when we reach depth n *)
  let fold_df g n f a =
    (* starting at generator l *)
    let rec explore l d a =
      if d < n then
        for
          (* with next reduced generator *)
          i = l - 1 to l + 1
        do
          let k = gcla g i in
          (* apply f on l and accumulator depth first *)
          explore k (d + 1) (f l a)
        done in
    (* for each generator *)
    for r = 0 to glen g - 1 do
      explore r 0 a
    done;
    a

  let test n =
    let g = { generators = "aBAb" } in
    fold_df g n (fun l t -> Mobius.compose t Mobius.identity) Mobius.j
end
