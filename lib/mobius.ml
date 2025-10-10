(** Mobius transformations using the extended complex plane *)

(* circles and lines a.k.a. clines *)
open Geometry
open Owl.Mat
open Owl_dense_matrix_z
open Owl_linalg_generic

(* encapsulate the implementation type *)
type t = mat

(* normalise a matrix *)
let normalise (m : mat) =
  let det = det m in
  let scale = Complex.(pow (inv det) { re = 0.5; im = 0.0 }) in
  scale $* m (* should ensure this is ad-bc = 1.0 *)

(* convenience constructors apply normalisation on construction only *)
let matrix a b c d = of_array [| a; b; c; d |] 2 2 |> normalise
let from_array a = of_array a 2 2

(* composition is matrix "multiplication" under the advice of my research
   assistant Khwarizmi we use QR decomposition to eliminate
   floating point rounding error accumulation. *)
let compose (m1 : mat) (m2 : mat) =
  let q1, r1, _ = qr m1 in
  q1 *@ (r1 *@ m2)

(* cross ratio invariant for testing *)
let cross_ratio m =
  let open Complex in
  let a = get m 0 0 in
  let b = get m 0 1 in
  let c = get m 1 0 in
  let d = get m 1 1 in
  div (mul (sub a c) (sub b d)) (mul (sub a d) (sub b c))

(* inverse transformations *)
let inverse m = inv m

(* apply transform to a point *)

(* transform a point in complex plane *)
let transform_point m z =
  let open Complex in
  let a = get m 0 0 in
  let b = get m 0 1 in
  let c = get m 1 0 in
  let d = get m 1 1 in
  (* if z = inf then div a c else *)
  let num = add (mul a z) b in
  let den = add (mul c z) d in
  div num den

(* transform a circle in complex plane *)
let transform_circle m r =
  let open Complex in
  let _a = get m 0 0 in
  let _b = get m 0 1 in
  let c = get m 1 0 in
  let d = get m 1 1 in

  let f2c x = { re = x; im = 0. } in
  let w = conj (add r.centre (div d c)) in
  let r2 = f2c (r.r *. r.r) in
  let z = sub r.centre (div r2 w) in
  let centre = transform_point m z in
  let radius = norm (sub centre (transform_point m (add r.centre (f2c r.r)))) in
  { centre; r = radius }

(* general fixed point formula - simplified assumed normal *)
let fixed_points m =
  let open Complex in
  let f2c x = { re = x; im = 0. } in
  let a = get m 0 0 in
  let _b = get m 0 1 in
  let c = get m 1 0 in
  let d = get m 1 1 in
  let alessd = sub a d in
  let tr2 = mul (trace m) (trace m) in
  let four = f2c 4.0 in
  let tr2less4 = sub tr2 four in
  let root2tr2l4 = sqrt tr2less4 in
  let two = f2c 2.0 in
  let two_c = mul two c in
  let r1 = div (sub alessd root2tr2l4) two_c in
  let r2 = div (add alessd root2tr2l4) two_c in
  (r1, r2)

(* this is useful *)
let identity = eye 2

let pp ppf m =
  let open Owl_pretty in
  pp_dsnda ppf m
