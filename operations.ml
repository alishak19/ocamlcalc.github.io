type exp = 
  | VAR of string
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp * exp

module SMap = Map.Make (String)

let rec aux_sigma var b env e =
  let rec aux env acc =
    let v = SMap.find var env in
    if v > b then acc
    else 
      let res = calculator env e in
      aux (SMap.add var (v + 1) env) (acc + res)
  in aux env 0

and calculator env = function
  | VAR x -> SMap.find x env
  | INT x -> x
  | ADD (e1, e2) -> calculator env e1 + calculator env e2
  | SUB (e1, e2) -> calculator env e1 - calculator env e2
  | MUL (e1, e2) -> calculator env e1 * calculator env e2
  | DIV (e1, e2) -> calculator env e1 / calculator env e2
  | SIGMA (var, e1, e2, e3) -> 
    match var with
    | VAR x ->
      let init = calculator env e1 in 
      let bound = calculator env e2 in
      aux_sigma x bound (SMap.add x init env) e3
    | _ -> assert false

let e1 = SIGMA (VAR "X", INT 1, INT 10, SUB(MUL(VAR "X", VAR "X"), INT 1))

let () = Printf.printf "%d\n" (calculator SMap.empty e1)
