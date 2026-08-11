let rec fix ?(eq = ( = )) f x =
  let y = f x in
  if eq x y then x else fix ~eq f y

  (* Oleg's polyvariadic fixed point combinator, 
     see here: https://okmij.org/ftp/Computation/fixed-point-combinators.html
  *)
let poly fs = 
  let rec knot f x = f (knot f) x in 
  knot (fun self fs -> List.map (fun f x -> f (self fs) x) fs) fs
