type token = Int_tok of float
           | Float_tok of float
           | Bool_tok of bool
           | Id_tok of string
           | Plus_tok 
           | Minus_tok
           | Times_tok 
           | Div_tok
           | Is_Equ_tok
           | Is_Neq_tok
           | Equ_tok 
           | If_tok
           | Else_tok
           | Lparen_tok
           | Rparen_tok
           | Lcomment_tok
           | Rcomment_tok
           | Print_tok
           | Geq_tok
           | Gth_tok
           | Leq_tok
           | Lth_tok
           | Exp_tok
           | And_tok
           | Or_tok
           | Nl_tok
           | Tab_tok
           | Space_tok
           | End_If_tok
;;

exception IgnoreCase;;
exception SyntaxError;;
exception NotDefined;;
exception DivisionByZero;;

let print_token = fun token ->
  match token with
    Int_tok a -> (
                   let _ = print_int (int_of_float a) in
                   print_string "\n" 
                 )
  | Float_tok a -> (
                     let _ = print_float a in
                     print_string "\n" 
                   )
  | Bool_tok a ->  (
                     if a then
                     Printf.printf "True\n"
                     else
                     Printf.printf "False\n"
                   )
  | Id_tok a ->    (
                     let _ = Printf.printf "Id_tok " in
                     let _ = Printf.printf "\"" in
                     let _ = Printf.printf "%s" a in
                     Printf.printf "\"" 
                   )
  | Plus_tok ->     ( Printf.printf "Plus_tok")
  | Minus_tok ->    ( Printf.printf "Minus_tok")
  | Div_tok ->      ( Printf.printf "Div_tok")
  | Times_tok ->    ( Printf.printf "Times_tok")
  | Is_Equ_tok ->   ( Printf.printf "Is_Equ_tok")
  | Is_Neq_tok ->   ( Printf.printf "Is_Neg_tok")
  | Equ_tok ->      ( Printf.printf "Equ_tok")
  | If_tok ->       ( Printf.printf "If_tok")
  | Else_tok ->     ( Printf.printf "Else_tok")
  | Lparen_tok ->   ( Printf.printf "Lparen_tok")
  | Rparen_tok ->   ( Printf.printf "Rparen_tok")
  | Lcomment_tok -> ( Printf.printf "Lcomment_tok")
  | Rcomment_tok -> ( Printf.printf "Rcomment_tok")
  | Print_tok ->    ( Printf.printf "Print_tok")
  | Geq_tok ->      ( Printf.printf "Geq_tok")
  | Gth_tok ->      ( Printf.printf "Gth_tok")
  | Leq_tok ->      ( Printf.printf "Leq_tok") 
  | Lth_tok ->      ( Printf.printf "Lth_tok")
  | Exp_tok ->      ( Printf.printf "Exp_tok")
  | And_tok ->      ( Printf.printf "And_tok")
  | Or_tok ->       ( Printf.printf "Or_tok")
  | Nl_tok ->       ( Printf.printf "Nl_tok")
  | Tab_tok ->      ( Printf.printf "Tab_tok")
  | Space_tok ->    ( Printf.printf "Space_tok")
  | End_If_tok ->   ( Printf.printf "End_If_tok")
;;

let rec go_through = fun tokens ->
  match tokens with
    x::xs ->
    (
      let _ = print_token x in
      if xs <> []
      then
        let _ = print_string ", " in
        go_through xs 
      else
        go_through xs 
    )
  |  [] -> ()
;;

let print_tokens = fun tokens ->
  let _ = Printf.printf "[" in
  let _ = go_through tokens in
  Printf.printf "]\n"
;;
