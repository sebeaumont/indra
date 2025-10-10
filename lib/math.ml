(* Math for Indra's pearls and all things to be found in the complex plane
   Copyleft 2025 Simon Beaumont, IoW, England *)

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

  (* this is how we define ComplexExt.nan *)
  let is_nan (z : Complex.t) = Float.is_nan z.re && Float.is_nan z.re

  (* convenience constructor *)
  let z r i = { re = r; im = i }
  let ( ~~ ) = Complex.neg
  let ( ~. ) = Float.neg

  (* nota bene a Float.nan cannot be compared as a number *)
  let infinity = z Float.infinity Float.zero
  let nan = z Float.nan Float.nan
  let f2c f = z f Float.zero

  (* floating point equality - since we are normalised then this should
     be ok for big enough eps *)
  let almost_equal ?(eps = Float.epsilon) a b =
    let d = Float.abs (a -. b) in
    d <= eps

  let almost_zero ?(eps = 4.0 *. Float.epsilon) { re; im } =
    almost_equal ~eps re 0.0 && almost_equal ~eps im 0.0

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

  let to_string { re; im } =
    let sign = if im >= 0.0 then "+" else "" in
    Printf.sprintf "(%.4G%s%.4Gi)" re sign im

  let pp ppf { re; im } =
    let sign = if im >= 0.0 then "+" else "" in
    Format.fprintf ppf "(%f%s%fi)" re sign im
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

  exception Singular of t

  (* check for singularity and normalise so det = 1 and an acceptable
     element of the PSL(2,C) or Kleinian group *)
  let normalise m =
    let det = determinant m in
    if det = zero then raise (Singular m)
    else
      (* could be -ve or +ve and hence special *)
      let uniter = sqrt det in
      {
        a = divx m.a uniter;
        b = divx m.b uniter;
        c = divx m.c uniter;
        d = divx m.d uniter;
      }

  (* convenience constructor apply normalisation *)
  let matrix a b c d = { a; b; c; d } |> normalise

  (* composition is "matrix" multiplication under the advice of my
   assistant we are going to use QR decomosition to *)
  let compose m1 m2 =
    {
      a = addx (mulx m1.a m2.a) (mulx m1.b m2.c);
      b = addx (mulx m1.a m2.b) (mulx m1.b m2.d);
      c = addx (mulx m1.c m2.a) (mulx m1.d m2.c);
      d = addx (mulx m1.c m2.b) (mulx m1.d m2.d);
    }
  (* |> normalise should not be necessary but causus test failure! *)

  let trace m = addx m.a m.d
  let inverse m = { m with a = neg m.d; d = neg m.a }

  (* transform a point in complex plane *)
  let transform_point m z =
    if z = infinity then divx m.a m.c
    else
      let num = addx (mulx m.a z) m.b in
      let den = addx (mulx m.c z) m.d in
      if almost_zero den then infinity else divx num den

  (* transform a circle in complex plane *)
  let transform_circle m c =
    let w = conj (addx c.centre (divx m.d m.c)) in
    let r2 = f2c (c.r *. c.r) in
    let z = subx c.centre (divx r2 w) in
    let centre = transform_point m z in
    let radius =
      norm (subx centre (transform_point m (addx c.centre (f2c c.r)))) in
    { centre; r = radius }

  (* useful transformations *)
  let identity = matrix one zero zero one
  let j = matrix zero i i zero

  (* sign is irrelevant in special group *)
  let is_normal m = almost_equal (norm2 (determinant m)) Float.one

  (* general fixed point formula - simplified assumed normal *)
  let fixed_points m =
    let alessd = subx m.a m.d in
    let tr2 = mulx (trace m) (trace m) in
    let four = f2c 4.0 in
    let tr2less4 = subx tr2 four in
    let root2tr2l4 = sqrt tr2less4 in
    let two = f2c 2.0 in
    let two_c = mulx two m.c in
    let r1 = divx (subx alessd root2tr2l4) two_c in
    let r2 = divx (addx alessd root2tr2l4) two_c in
    (r1, r2)

  (* printer *)
  let to_string { a; b; c; d } =
    "\n|"
    ^ to_string a
    ^ "; "
    ^ to_string b
    ^ "|\n|"
    ^ to_string c
    ^ "; "
    ^ to_string d
    ^ "|\n"
end

(* -------------------------------------------------------------------------------- *)

module Group = struct
  type t = {
    generators : String.t;
    transformations : Mobius.t array;
    fixpoints : (ComplexExt.t * ComplexExt.t) array;
  }

  (** Nota Bene: We rely on a certain ordering of generator letters for
      efficient adjacent inverse avoidance in reduced form: e.g. "abAB" avoids
      such if we only look to our immediate neighbours and ourselves as valid
      next letters (cyclically). However to ensure nice generation behaviour on
      depth first traversal of the "tree" we cyclically permute the letters
      anti-clockwise to get "aBAb". Whatever you give us here this should
      provide something at least useable but as this was intended to be a two
      generator group then the default "ab" is common usage. *)

  let make groupspec =
    let keys, txs = List.split groupspec in
    let ivs = List.map Mobius.inverse txs in
    let letk = List.map Char.lowercase_ascii keys in
    let invk = List.map Char.uppercase_ascii keys in

    (* build new assoc list with inverses added in given order *)
    let mmap = List.combine (List.append letk invk) (List.append txs ivs) in

    (* rationalise the letters and do cyclic permutation with inverses *)
    let letters = List.to_seq keys |> String.of_seq in
    (* sort uniq in dictionary l/c and append the inverse letters *)
    let l =
      String.to_seq letters
      |> List.of_seq
      |> List.sort_uniq Char.compare
      |> List.to_seq
      |> String.of_seq in
    let gdict = String.cat l (String.uppercase_ascii l) in
    (* do an anti-clockwise cyclic permutation so dfs works nicely *)
    let cycle_ac x =
      let len = String.length x in
      String.init len (function
        | 0 -> String.get x 0
        | n -> String.get x (len - n)) in
    (* build array of transformations in permuted order for O(1) lookups *)
    let gens = cycle_ac gdict in
    let mtrx =
      Array.init (String.length gens) (fun i ->
          List.assoc (String.get gens i) mmap) in
    let fixp =
      Array.init (Array.length mtrx) (fun i -> Mobius.fixed_points mtrx.(i))
    in
    { generators = gens; transformations = mtrx; fixpoints = fixp }

  (* utilities TODO rationalise *)
  let glen g = String.length g.generators

  (* todo combine these two now *)
  let gcla g i = if i < 0 then glen g + i else i mod glen g
  let goff g = (glen g / 2) - 1 (* glen is always even *)
  let transformation g l = g.transformations.(l)
  let multiply_left g l t = Mobius.compose g.transformations.(l) t
  let multiply_right g l t = Mobius.compose t g.transformations.(l)

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

  (* TODO: lookup matrix for group letter *)

  (** Fold depth first over group generators stopping when we reach maxdepth
      also provide for a halting predicate which is applied to the result of
      applying the accumulated transformation to the relevant fixed point for
      the current generator, (maxdepth may need to be large in this case). *)

  (* we need one function to compute the operator, usually by composing
     tranformations on the left as we explore the group tree and another
     action on the result... *)

  let action f r = f r

  let fold_df ~group:g ~maxdepth:n
      ?until:(p =
          function
          | _ -> false)
      (*
        ?operator:(m = Mobius.compose)
        ?action:(r = fun o -> a
   *)
        f a =
    (* explore tree starting at generator l *)
    let rec explore l d a =
      if d < n || p a then
        for
          (* with next reduced generator *)
          i = l - goff g to l + goff g
        do
          let k = gcla g i in
          (* apply f on l, transform and result accumulators *)
          explore k (d + 1) (f l a)
        done in
    (* for each generator *)
    for r = 0 to glen g - 1 do
      explore r 0 a
    done;
    a
end
