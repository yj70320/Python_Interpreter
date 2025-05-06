#use "token.ml";;

type 'a parse_tree = Empty
                   | Node of 'a parse_tree * 'a * 'a parse_tree
;;

(* the next few functions do operations on the values found in the trees *)
let add_values = fun left -> fun right -> 
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b -> (Int_tok (a +. b))
      | Float_tok b -> (Float_tok (a +. b))
      | Bool_tok b ->
        (
          if b = true then (Int_tok (a +. 1.0)) else (Int_tok (a +. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b -> (Float_tok (a +. b))
      | Float_tok b -> (Float_tok (a +. b))
      | Bool_tok b ->
        (
          if b = true then (Float_tok (a +. 1.0)) else (Float_tok (a +. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = true then (Int_tok (1.0 +. b)) else raise DivisionByZero
        )
      | Float_tok b ->
        (
          if a = true then (Float_tok (1.0 +. b)) else raise DivisionByZero
        )
      | Bool_tok b ->
        (
          if a = true && b = true then (Int_tok (1.0 +. 1.0))
          else
          if a = false && b = true then (Int_tok (0.0 +. 1.0))
          else
          if a = true && b = false then (Int_tok (1.0 +. 0.0))
          else
            (Int_tok (0.0 +. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let minus_values = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b -> (Int_tok (a -. b))
      | Float_tok b -> (Float_tok (a -. b))
      | Bool_tok b ->
        (
          if b = true then (Int_tok (a -. 1.0)) else (Int_tok (a -. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b -> (Float_tok (a -. b))
      | Float_tok b -> (Float_tok (a -. b))
      | Bool_tok b ->
        (
          if b = true then (Float_tok (a -. 1.0)) else (Float_tok (a -. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = true then (Int_tok (1.0 -. b)) else raise DivisionByZero
        )
      | Float_tok b ->
        (
          if a = true then (Float_tok (1.0 -. b)) else raise DivisionByZero
        )
      | Bool_tok b ->
        (
          if a = true && b = true then (Int_tok (1.0 -. 1.0))
          else
          if a = false && b = true then (Int_tok (0.0 -. 1.0))
          else
          if a = true && b = false then (Int_tok (1.0 -. 0.0))
          else
            (Int_tok (0.0 -. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let times_values = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b -> (Int_tok (a *. b))
      | Float_tok b -> (Float_tok (a *. b))
      | Bool_tok b ->
        (
          if b = true then (Int_tok (a *. 1.0)) else (Int_tok (a *. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b -> (Float_tok (a *. b))
      | Float_tok b -> (Float_tok (a *. b))
      | Bool_tok b ->
        (
          if b = true then (Float_tok (a *. 1.0)) else (Float_tok (a *. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = true then (Int_tok (1.0 *. b)) else (Int_tok (0.0))
        )
      | Float_tok b ->
        (
          if a = true then (Float_tok (1.0 *. b)) else (Float_tok (0.0))
        )
      | Bool_tok b ->
        (
          if a = true && b = true then (Int_tok (1.0 *. 1.0))
          else
          if a = false && b = true then (Int_tok (0.0 *. 1.0))
          else
          if a = true && b = false then (Int_tok (1.0 *. 0.0))
          else
            (Int_tok (0.0 *. 0.0))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let div_values = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b -> if b <> 0.0 then (Int_tok (a /. b)) else raise DivisionByZero
      | Float_tok b -> if b <> 0.0 then (Float_tok (a /. b))
        else raise DivisionByZero
      | Bool_tok b ->
        (
          if b = true then (Int_tok (a /. 1.0)) else raise DivisionByZero
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b -> if b <> 0.0 then (Float_tok (a /. b))
        else raise DivisionByZero
      | Float_tok b -> if b <> 0.0 then (Float_tok (a /. b))
        else raise DivisionByZero
      | Bool_tok b ->
        (
          if b = true then (Float_tok (a /. 1.0)) else raise DivisionByZero
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = true then
            if b <> 0.0 then (Int_tok (1.0 /. b)) else raise DivisionByZero
          else
            (Int_tok (0.0 /. b))
        )
      | Float_tok b ->
        (
          if a = true then
            if b <> 0.0 then (Float_tok (1.0 /. b)) else raise DivisionByZero
          else
            (Float_tok (0.0 /. b))
        )
      | Bool_tok b ->
        (
          if a = true && b = true then (Int_tok (1.0 /. 1.0))
          else
          if a = false && b = true then (Int_tok (0.0 /. 1.0))
          else
            raise DivisionByZero
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

(* this will do our exponentiation *)
let exp_values = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      let _ = print_string "here\n" in 
      match right with
        Int_tok b -> let _ = print_float a in
        let _ = print_float b in (Int_tok (a ** b))
      | Float_tok b -> (Float_tok (a ** b))
      | Bool_tok b ->
        (
          if b = true then (Int_tok (a ** 1.))
          else (Int_tok (a ** 0.))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      let _ = print_string "float\n" in 
      match right with
        Int_tok b -> (Float_tok (a ** b))
      | Float_tok b -> (Float_tok (a ** b))
      | Bool_tok b ->
        (
          if b = true then (Float_tok (a ** 1.))
          else (Float_tok (a ** 0.))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = true then (Int_tok (1. ** b))
          else (Int_tok (0. ** b))
        )
      | Float_tok b ->
        (
          if a = true then (Float_tok (1. ** b))
          else (Float_tok (0. ** b))
        )
      | Bool_tok b ->
        (
          if a = true && b = true then (Int_tok (1. ** 1.))
          else
          if a = false && b = true then (Int_tok (0. ** 1.))
          else
          if a = true && b = false then (Int_tok (1. ** 0.))
          else
            (Int_tok (0. ** 0.))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let is_equ = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a = b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a = 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a = 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a = b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a = b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a = 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a = 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        ( 
          if a then
            if 1.0 = b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 = b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a then
            if 1.0 = b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 = b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if a && b then
            (Bool_tok (true)) 
          else
          if a = false && b = false then
            (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let not_equ = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a <> b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a <> b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a <> 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a <> 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a <> b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a <> b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a <> 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a <> 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a then
            if 1.0 <> b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 <> b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a then
            if 1.0 <> b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 <> b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if a && b then
            (Bool_tok (false)) 
          else
          if a = false && b = false then
            (Bool_tok (false)) else (Bool_tok (true))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let geq = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a >= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a >= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a >= 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a >= 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a >= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a >= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a >= 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a >= 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a then
            if 1.0 >= b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 >= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a then
            if 1.0 >= b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 >= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if a then
            (Bool_tok (true)) 
          else
          if a = false && b = false then
            (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let gth = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a > b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a > b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a > 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a > 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a > b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a > b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a > 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a > 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a then
            if 1.0 > b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 > b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a then
            if 1.0 > b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 > b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if a && b = false then
            (Bool_tok (true)) 
          else
            (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let leq = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a <= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a <= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a <= 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a <= 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a <= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a <= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a <= 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a <= 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a then
            if 1.0 <= b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 <= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a then
            if 1.0 <= b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 <= b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if a = false then
            (Bool_tok (true)) 
          else
            if a && b then (Bool_tok (true)) else (Bool_tok (false))      
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let lth = fun left -> fun right ->
  match left with
    Int_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a < b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a < b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a < 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a < 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Float_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a < b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a < b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if b then
            if a < 1.0 then (Bool_tok (true)) else (Bool_tok (false))
          else
            if a < 0.0 then (Bool_tok (true)) else (Bool_tok (false))
        )
      | _ -> raise SyntaxError
    )
  | Bool_tok a ->
    (
      match right with
        Int_tok b ->
        (
          if a then
            if 1.0 < b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 < b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Float_tok b ->
        (
          if a then
            if 1.0 < b then (Bool_tok (true)) else (Bool_tok (false))
          else
            if 0.0 < b then (Bool_tok (true)) else (Bool_tok (false))
        )
      | Bool_tok b ->
        (
          if a then
            (Bool_tok (false)) 
          else
            if b then (Bool_tok (true)) else (Bool_tok (false))      
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

(* this creates our and operator *)
let nd = fun left -> fun right ->
  match left with
    Bool_tok a ->
    (
      match right with
        Bool_tok b ->
        (
          if a && b then
            (Bool_tok (true)) 
          else
            (Bool_tok (false))      
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

let or_ = fun left -> fun right ->
  match left with
    Bool_tok a ->
    (
      match right with
        Bool_tok b ->
        (
          if a || b then
            (Bool_tok (true)) 
          else
            (Bool_tok (false))      
        )
      | _ -> raise SyntaxError
    )
  | _ -> raise SyntaxError
;;

(* this will return the value of our tree *)
let evaluate = fun left -> fun right -> fun root ->
  match root with
    Plus_tok ->
    (
      add_values left right
    )
  | Minus_tok ->
    (
      minus_values left right
    )
  | Div_tok ->
    (
      div_values left right
    )
  | Times_tok ->
    (
      times_values left right
    )
  | Exp_tok ->
    (
      exp_values left right
    )
  | Is_Equ_tok ->
    ( 
      is_equ left right
    )
  | Is_Neq_tok ->
    (
      not_equ left right
    )
  | Geq_tok ->
    (
      geq left right
    )
  | Gth_tok ->
    (
      gth left right
    )
  | Leq_tok ->
    (
      leq left right
    )
  | Lth_tok ->
    (
      lth left right
    )
  | And_tok ->
    (
      nd left right
    )
  | Or_tok ->
    (
      or_ left right
    )
  | _ -> raise SyntaxError
;;

(* we are doing left, right, root to evaluate the tree *)
let rec post_order = fun node ->
  match node with
    Node (left, root, right) when left = Empty || right = Empty ->
    (
      root
    )
  | Node (left, root, right) ->
    (
      let left_ = post_order left in
      let right_ = post_order right in
      let root_ = root in
      evaluate left_ right_ root_
    )
  | Empty -> raise IgnoreCase
;;
