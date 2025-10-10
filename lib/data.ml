(* data mainly for testing, seed values, interesting parameters etc. *)
module CC = Complex
module M = Mobius

(* root2 *)
let r2 = Float.sqrt 2.0

(* elliptic *)
let ea = { CC.re = r2 /. 2.; im = 0. }
let eb = { CC.re = r2 /. -2.; im = 0. }
let ec = { CC.re = r2 /. 2.; im = 0. }
let ed = { CC.re = r2 /. 2.; im = 0. }

(* hyperbolic *)
let ha = { CC.re = 2.; im = 0. }
let hb = { CC.re = 0.; im = 0. }
let hc = { CC.re = 0.; im = 0. }
let hd = { CC.re = 1.; im = 0. }

(* parabolic *)
let pa = { CC.re = 1.; im = 0. }
let pb = { CC.re = 1.; im = 0. }
let pc = { CC.re = 0.; im = 0. }
let pd = { CC.re = 1.; im = 0. }

(* transformations *)
let me = M.matrix ea eb ec ed
let mh = M.matrix ha hb hc hd
let mp = M.matrix pa pb pc pd
