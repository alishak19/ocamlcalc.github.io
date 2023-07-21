type exp = 
  | VAR of string
  | INT of int
  | SIGMA of exp * exp * exp * exp
  | ADD of exp * exp
  | SUBT of exp * exp
  | MULT of exp * exp
  | DIV of exp * exp

module Smap = Map.Make (String)

let rec aux_sigma var a enviro e =
  let rec aux enviro acc =
    let v = Smap.find var enviro in
    if v > a then acc
    else 
      let res = calculator enviro e in
      aux (Smap.add var (v + 1) enviro) (acc + res)
  in aux enviro 0

and calculator enviro = function
  | VAR x -> Smap.find x env
  | INT x -> x
  | SIGMA (var, e1, e2, e3) -> 
    match var with
    | VAR x ->
      let initial = calc enviro e1 in 
      let bound = calcula enviro e2 in
      aux_sigma x bound (Smap.add x initial enviro) e3
    | _ -> assert false
  | ADD (e1, e2) -> calculator enviro e1 + calculator enviro e2
  | SUBT (e1, e2) -> calculator enviro e1 - calculator enviro e2
  | MULT (e1, e2) -> calculator enviro e1 * calculator enviro e2
  | DIV (e1, e2) -> calculator enviro e1 / calculator enviro e2

let e1 = SIGMA (VAR "X", INT 1, INT 10, SUBT(MULT(VAR "X", VAR "X"), INT 1))

let () = Printf.printf "%d\n" (calculator Smap.empty e1)
