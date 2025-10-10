(** Complex numbers extended. We extend the complex numbers to include the point
    at infinity and provide extended arithmetic. *)

(* probably deprecated *)

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
