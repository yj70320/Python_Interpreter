#use "lexer.mml.ml";;

exception NotDefined;;
exception ExpectedBlock;;
exception IndentionError;;
exception UnexpectedIndent;;


let is_op = fun token ->
  match token with
    Plus_tok -> true
  | Minus_tok -> true
  | Times_tok -> true
  | Div_tok -> true
  | Is_Equ_tok -> true
  | Is_Neq_tok -> true
  | Equ_tok -> false
  | Lparen_tok -> true
  | Rparen_tok -> true
  | Geq_tok -> true
  | Gth_tok -> true
  | Leq_tok -> true
  | Lth_tok -> true
  | Exp_tok -> true
  | And_tok -> true
  | Or_tok -> true
  | _ -> false
;;

let e_s () =
  print_string "ERROR: Syntax Error\n"
;;

let e_i () =
  print_string "ERROR: Ignored Value\n"
;;

let e_d () =
  print_string "ERROR: Variable Not Defined\n"
;;

let e_c () =
  print_string "ERROR: Bad Character Found\n"
;;

let e_z () =
  print_string "ERROR: Division By Zero!\n"
;;

let e_b () =
  print_string "ERROR: Expected An Indented Block\n"
;;

let e_l () =
  print_string "ERROR: Indention Levels Do Not Match\n"
;;

let e_u () =
  print_string "ERROR: Unexpected Indent\n"
;;



let is_num = fun token ->
  match token with
    Int_tok a -> true
  | Float_tok a -> true
  | _ -> false
;;

(* checks to see if token is Id_tok *)
let is_id = fun token ->
  match token with
    Id_tok s -> true
  | _ -> false
;;

let is_print = fun token ->
  match token with
    Print_tok -> true
  | _ -> false
;;

let is_if = fun token_list ->
  match token_list with
    If_tok -> true
  | _ -> false       
;;

let is_space = fun token ->
  match token with
    Nl_tok -> true
  | Tab_tok -> true
  | Space_tok -> true
  | _ -> false
;;

let is_bool = fun token ->
  match token with
    Bool_tok b -> true
  | _ -> false
;;

let is_indented = fun tokens ->
  match tokens with
    x::xs ->
    (
      match x with
        Nl_tok -> true
      | Tab_tok -> true
      | Space_tok -> true
      | _ -> false
    )
  | [] -> false
;;

let is_else = fun tokens ->
  match tokens with
    x::xs ->
    (
      match x with
        Else_tok ->
        (
          match xs with
            y::ys ->
            (
              match y with
                End_If_tok -> if ys = [] then true else raise SyntaxError
              | _ -> false
            )
          | [] -> raise SyntaxError
        )
      | _ -> false
    )
  | [] -> false
;;

let nd = fun token -> fun op_stack ->
  match op_stack with
    x::xs ->
    (
      match token with
        Is_Equ_tok ->
        (
          match x with
            Is_Equ_tok -> true
          | Is_Neq_tok -> true
          | Geq_tok -> true
          | Gth_tok -> true
          | Leq_tok -> true
          | Lth_tok -> true
          | _ -> false
        )
      | Is_Neq_tok ->
        (
          match x with
            Is_Equ_tok -> true
          | Is_Neq_tok -> true
          | Geq_tok -> true
          | Gth_tok -> true
          | Leq_tok -> true
          | Lth_tok -> true
          | _ -> false
        )
      | Geq_tok ->
        (
          match x with
            Is_Equ_tok -> true
          | Is_Neq_tok -> true
          | Geq_tok -> true
          | Gth_tok -> true
          | Leq_tok -> true
          | Lth_tok -> true
          | _ -> false
        )
      | Gth_tok ->
        (
          match x with
            Is_Equ_tok -> true
          | Is_Neq_tok -> true
          | Geq_tok -> true
          | Gth_tok -> true
          | Leq_tok -> true
          | Lth_tok -> true
          | _ -> false
        )
      | Leq_tok ->
        (
          match x with
            Is_Equ_tok -> true
          | Is_Neq_tok -> true
          | Geq_tok -> true
          | Gth_tok -> true
          | Leq_tok -> true
          | Lth_tok -> true
          | _ -> false
        )
      | Lth_tok ->
        (
          match x with
            Is_Equ_tok -> true
          | Is_Neq_tok -> true
          | Geq_tok -> true
          | Gth_tok -> true
          | Leq_tok -> true
          | Lth_tok -> true
          | _ -> false
        )
      | _ -> false
    )
  | [] -> false
;;
(* this function tells us whether or not there is more after the Rparen_tok that
   we should find. If there is error, if theres not rparen, error, otherwise we
   are good! *)
let rec search_print = fun tokens ->
  match tokens with
    x::xs ->
    (
      match x with
        Rparen_tok ->
        (
          match xs with
            y::ys -> raise SyntaxError
          | [] -> true
        )
      | _ -> search_print xs
    )
  | [] -> raise SyntaxError
;;

(* this function tells us whether or not there is more after the End_If_tok that
   we should find. No end = error, otherwise we are good! *)
let rec search_if = fun tokens ->
  match tokens with
    x::xs ->
    (
      match x with
        End_If_tok ->
        (
          match xs with
            y::ys -> raise SyntaxError
          | [] -> true
        )
      | _ -> search_if xs
    )
  | [] -> raise SyntaxError
;;

(* this function will tell us whether or not a token should be int or float *)
let is_float = fun lvalue -> fun rvalue ->
  match lvalue with
    Int_tok a ->
    (
      match rvalue with
        Int_tok a -> false
      | Float_tok a -> true
      | _ -> raise SyntaxError
    )
  | Float_tok a -> true
  | _ -> raise SyntaxError
;;

(* makes a node out of our value *)
let create_node = fun token ->
  match token with
    Int_tok a -> Node (Empty, (Int_tok a), Empty)
  | Float_tok a -> Node (Empty, (Float_tok a), Empty)
  | Bool_tok b -> Node (Empty, (Bool_tok b), Empty)
  | _ -> raise SyntaxError
;;

(* this returns a tuple. This tuple tells us whether or not the lexed part is
   wanting to print the value of a variable or if it is an assignment *)
let check_assignment = fun token_list ->
  match token_list with
    x::xs ->
    (
      match x with
        Id_tok s ->
        (
          match xs with
            y::ys ->
            (
              match y with
                Equ_tok -> (true, false)
              | _ -> (false, false)
            )
          | [] -> (false, true)
        )
      | Nl_tok -> raise IndentionError
      | Tab_tok -> raise IndentionError
      | Space_tok -> raise IndentionError
      | _ -> (false, false)
    )
  | [] -> raise SyntaxError
;;

(* removes all of the spaces from x *)
let rec fix_x = fun token_list -> fun acc ->
  match token_list with
    x::xs ->
    (
      match x with
        Space_tok ->
        (
          fix_x xs acc
        )
      | Nl_tok ->
        (
          fix_x xs acc
        )
      | Tab_tok ->
        (
          fix_x xs acc
        )
      | _ -> fix_x xs (acc @ [x])
    )
  | [] -> acc
;;

(* get the name of the variable *)
let get_var_name = fun token_list ->
  match token_list with
    x::xs ->
    (
      match x with
        Id_tok s -> s
      | _ -> raise SyntaxError
    )
  | [] -> raise SyntaxError
;;

(* this function returns a token based on what was stored in the variable
   table *)
let rec get_var_val = fun name_list -> fun name ->
  match name_list with
    x::xs ->
    (
      match x with
        (n, v) when name = n ->
        (
          v 
        )
      | (n, v) -> get_var_val xs name
    )
  | [] -> raise NotDefined
;;

(* we know its assignment, so take out Id_tok and Equ_tok *)
let change_tokens = fun token_list ->
  match token_list with
    x::xs ->
    (
      match xs with
        y::ys -> ys
      | [] -> raise SyntaxError
    )
  | [] -> raise SyntaxError
;;

(* this function goes through our name list and prints the first variable
   with the name it can find. Otherwise it is an error *)
let rec print_var = fun name_list -> fun var ->
  match name_list with
    x::xs ->
    (
      match x with
        (name, v) when var = name ->
        (
          match v with
            Int_tok a -> let _ = print_int (int_of_float a) in print_string "\n"
          | Float_tok a -> let _ = print_float a in print_string "\n"
          | Bool_tok a -> print_token v
          | _ -> raise SyntaxError
        )
      | _ -> print_var xs var
    )
  | [] -> raise NotDefined
;;

(* makes a node out of our Id *)
let create_node_id = fun token ->
  match token with
    Id_tok s -> Node (Empty, (Id_tok s), Empty)
  | _ -> raise SyntaxError
;;

(* we found an operator to make a node out of. This means we need 2 nums and
the op from the top of the op_stack*)
let create_node_op = fun left -> fun op -> fun right ->
  Node (left, op, right)
;;

(* returns the top of our op_stack *)
let get_top_op = fun op_stack ->
  match op_stack with
    x::xs -> x
  | [] -> raise SyntaxError
;;

let is_rparen = fun op_token ->
  match op_token with
    Rparen_tok -> true
  | _ -> false
;;

(* returns what we want our left node to be *)
let left_ = fun node_stack ->
  match node_stack with
    x::xs ->
    (
      match xs with
        y::ys -> y
      | [] -> raise SyntaxError
    )
  | [] -> raise SyntaxError
;;

(* this function gets rid of the : in if statement booleans *)
let rec fix_if = fun tokens -> fun acc ->
  match tokens with
    x::xs ->
    (
      match x with
        End_If_tok -> acc
      | _ -> fix_if xs (acc @ [x])
    )
  | [] -> acc
;;

(* removes the top op from our op_stack *)
let remove_top = fun op_stack ->
match op_stack with
    x::xs ->
    (
      xs
    )
  | [] -> raise SyntaxError
;;

(* removes the top 2 values from our node_stack *)
let remove_top_2 = fun node_stack ->
match node_stack with
    x::xs ->
    (
      match xs with
        y::ys -> ys
      | [] -> raise SyntaxError
    )
  | [] -> raise SyntaxError
;;

let rec remove_til_lparen = fun op_stack ->
  match op_stack with
    x::xs ->
    (
      match x with
        Lparen_tok -> xs
      | _ -> remove_til_lparen xs
    )
  | [] -> raise SyntaxError
;;

(* returns what we want our left node to be *)
let right_ = fun node_stack ->
  match node_stack with
    x::xs -> x
  | [] -> raise SyntaxError
;;

(* tells us whether or not the op on top of stack is leq to new op *)
let is_lesser = fun op_stack -> fun op_ ->
  match op_stack with
    x::xs ->
    (
      match x with
        Plus_tok when op_ = Plus_tok || op_ = Minus_tok
                      || op_ = Geq_tok || op_ = Gth_tok || op_ = Leq_tok
                      || op_ = Is_Equ_tok || op_ = Is_Neq_tok
                      || op_ = Leq_tok -> true
      | Plus_tok -> false
      | Minus_tok when op_ = Plus_tok || op_ = Minus_tok
                      || op_ = Geq_tok || op_ = Gth_tok || op_ = Leq_tok
                      || op_ = Is_Equ_tok || op_ = Is_Neq_tok
                      || op_ = Leq_tok -> true
      | Minus_tok -> false
      | Times_tok when op_ = Lparen_tok || op_ = Rparen_tok
                       || op_ = Exp_tok -> false
      | Times_tok -> true
      | Div_tok when op_ = Lparen_tok || op_ = Rparen_tok
                     || op_ = Exp_tok -> false
      | Exp_tok when op_ = Lparen_tok || op_ = Rparen_tok
                     || op_ = Exp_tok -> false
      | Exp_tok -> true
      | Div_tok -> true
      | Lparen_tok -> false
      | Rparen_tok -> false
      | Equ_tok -> false
      | Is_Equ_tok when op_ = Is_Equ_tok || op_ = And_tok
                        || op_ = Or_tok -> true
      | Is_Equ_tok -> false
      | Is_Neq_tok when op_ = Is_Equ_tok || op_ = Is_Neq_tok
                        || op_ = And_tok || op_ = Or_tok -> true
      | Is_Neq_tok -> false
      | Geq_tok when op_ = Is_Equ_tok || op_ = Is_Neq_tok
                     || op_ = Geq_tok || op_ = And_tok || op_ = Or_tok -> true
      | Geq_tok -> false
      | Gth_tok when op_ = Gth_tok || op_ = Geq_tok || op_ = Is_Equ_tok
                     || op_ = Is_Neq_tok || op_ = And_tok
                     || op_ = Or_tok -> true
      | Gth_tok -> false
      | Leq_tok when op_ = Leq_tok || op_ = Geq_tok || op_ = Gth_tok
                     || op_ = Is_Equ_tok
                     || op_ = Is_Neq_tok || op_ = And_tok
                     || op_ = Or_tok -> true
      | Leq_tok -> false
      | Lth_tok when op_ = Lth_tok || op_ = Geq_tok || op_ = Gth_tok
                     || op_ = Leq_tok
                     || op_ = Is_Equ_tok || op_ = Is_Neq_tok
                     || op_ = And_tok || op_ = Or_tok -> true
      | Lth_tok -> false
      | And_tok -> false
      | Or_tok -> false
      | _ -> false
    )
  | [] -> false
;;


(* this function will create a node based of what is found in between our paren, or it raise and error if there is no Lparen_tok *)
let rec eval_rparen = fun op_stack -> fun node_stack ->
  match op_stack with
    x::xs ->
    (
      match x with
        Lparen_tok -> node_stack
      | _ ->
        (
          let left = left_ node_stack in
          let right = right_ node_stack in
          let new_node = create_node_op left x right in
          let updated_node_stack = remove_top_2 node_stack in
          eval_rparen xs (new_node :: updated_node_stack)
        )
    )
  | [] -> raise SyntaxError
;;

(* this function will clear out whatever is left in our stacks *)
let rec finish_stacks = fun op_stack -> fun node_stack ->
  match op_stack with
    x::xs ->
    (
      match x with
        Lparen_tok -> raise SyntaxError
      | _ ->
        (
          let left = left_ node_stack in
          let right = right_ node_stack in
          let new_node = create_node_op left x right in
          let updated_node_stack = remove_top_2 node_stack in
          finish_stacks xs (new_node :: updated_node_stack)
        )
    )
  | [] ->
    (
      match node_stack with
        x::xs ->
        (
          match xs with
            y::ys -> raise SyntaxError
          | [] -> x
        )
      | [] -> raise SyntaxError
    )
;;

(* This function is going to exectute our shunting yard alg. Instead of
operating on everything though, will instead create a tree by making nodes and
putting them on the node stack. At the end of the proceedure, our node stack
should be a tree*)
let rec shunting_yard = fun token_list -> fun op_stack -> fun node_stack ->
  fun name_list ->
  match token_list with
    x::xs ->
    (
      if is_op x
      then
        (* check to see if op on top of stach is higher priority *)
        if is_lesser op_stack x
        (* see if we need to make changes to op stack *)
        then
          if nd x op_stack
          then
            let top_op = get_top_op op_stack in
            let left = left_ node_stack in
            let right = right_ node_stack in
            let new_node = create_node_op left top_op right in
            let updated_node_stack = remove_top_2 node_stack in
            let updated_op_stack = ((And_tok) :: (remove_top op_stack)) in
            shunting_yard xs (x::updated_op_stack)
              (right::new_node::updated_node_stack) name_list
          else
            let top_op = get_top_op op_stack in
            let left = left_ node_stack in
            let right = right_ node_stack in
            let new_node = create_node_op left top_op right in
            let updated_node_stack = remove_top_2 node_stack in
            let updated_op_stack = remove_top op_stack in
            shunting_yard xs (x::updated_op_stack)
              (new_node::updated_node_stack) name_list
        else
          (* we need to evaluate to be checking for Rparen *)
        if is_rparen x
        then
          (* we will combine everything on our stacks until Lparen is found *)
          let updated_node_stack = eval_rparen op_stack node_stack in
          match updated_node_stack with
            y::ys ->
            (
              let new_node = post_order y in
              let new_node = create_node new_node in
              let updated_op_stack = remove_til_lparen op_stack in
              shunting_yard xs updated_op_stack (new_node :: ys) name_list
            )
          | [] -> raise SyntaxError
        else
          (* could be dealing with a boolean expression, check top *)
        if nd x op_stack
        then
          let top_op = get_top_op op_stack in
          let left = left_ node_stack in
          let right = right_ node_stack in
          let new_node = create_node_op left top_op right in
          let updated_node_stack = remove_top_2 node_stack in
          let updated_op_stack = ((And_tok) :: (remove_top op_stack)) in
          shunting_yard xs (x::updated_op_stack)
            (right::new_node::updated_node_stack) name_list
        else
          shunting_yard xs (x::op_stack) node_stack name_list    
      else
      if is_num x
      (* just make a node and put it onto the stack *)    
      then
        let new_node = create_node x in
        shunting_yard xs op_stack (new_node :: node_stack) name_list
      else 
      if is_id x then
        (* we need to get the value stored in the variable*)
        match x with
          Id_tok s ->
          (
            let var = get_var_val name_list s in
            let new_node = create_node var in
            shunting_yard xs op_stack (new_node :: node_stack) name_list
          )
        | _ -> raise SyntaxError
      else
      if is_space x then
        shunting_yard xs op_stack node_stack name_list
      else
      if is_bool x
      then
        let new_node = create_node x in
        shunting_yard xs op_stack (new_node::node_stack)  name_list
      else
        raise SyntaxError
    )
  | [] -> finish_stacks op_stack node_stack 
;;

let compare_block = fun block -> fun statement ->
  match block with
    x::xs ->
    (
       match statement with
        y::ys ->
        (
          false
        )
      | [] -> true
    )
  | [] ->
    (
       match statement with
        y::ys ->
        (
          false
        )
      | [] -> true
    )
;;

let rec end_block = fun block -> fun statement ->
  match block with
    x::xs ->
    (
      if xs = [] then compare_block x statement
      else end_block xs statement
    )
  | [] -> false
;;

let rec execute_block = fun block -> fun name_list -> 
  match block with
    x::xs ->
    (
      let x = fix_x x [] in
      if x = [] then execute_block xs name_list
      else
        let assignment = check_assignment x in
        match assignment with
          (true, false) ->
          (
            let var_name = get_var_name x in
            let updated_token_list = change_tokens x in
            let p = shunting_yard updated_token_list [] [] name_list in
            let p = post_order p in
            (* now we need to assign the value to our var_name but we need the
               right type, we will use a boolean flag *)
            match p with
              Int_tok a ->
              (
                execute_block xs ((var_name, (Int_tok a)) :: name_list)
              )
            | Float_tok a ->
              (
                execute_block xs ((var_name, (Float_tok a)) :: name_list)
              )
            | Bool_tok b ->
              (
                if b then
                  execute_block xs ((var_name, (Bool_tok b)) :: name_list)
                else
                  execute_block xs ((var_name, (Bool_tok b)) :: name_list)
              )
            | _ -> raise SyntaxError
          )
        | (false, true) ->
          (
            (* try to print variable name, if it doesn't exist then error *)
            match x with
              y::ys ->
              (
                match y with
                  Id_tok s ->
                  (
                    let _ = print_var name_list s in
                    execute_block xs name_list
                  )
                | _ -> raise SyntaxError
              ) 
            | [] -> raise SyntaxError
          )
        | (false, false) ->
          (
            match x with
              y::ys ->
              (
                if is_print y
                then
                  let _ = search_print ys in
                  let p = shunting_yard ys [] [] name_list in
                  let p = post_order p in
                  let _ = print_token p in
                  execute_block xs name_list
                else
                  let p = shunting_yard x [] [] name_list in
                  let p = post_order p in
                  let _ = print_token p in
                  execute_block xs name_list 
              )
            | [] -> raise SyntaxError
          )
        | (_, _) -> raise SyntaxError
    )
  | [] -> name_list
;;

let rec indent = fun tokens -> fun acc ->
  match tokens with
    x::xs ->
    (
      match x with
        Nl_tok -> indent xs (acc @ [x])
      | Tab_tok -> indent xs (acc @ [x])
      | Space_tok -> indent xs (acc @ [x])
      | _ -> acc
    )
  | [] -> acc
;;

(* this will see if our current and previous blocks have same indent level *)
let rec indent_match = fun tokens -> fun indent_m ->
  match indent_m with
    x::xs ->
    (
      match x with
        Nl_tok ->
        (
          match tokens with
            y::ys ->
            (
              match y with
                Nl_tok -> indent_match ys xs
              | _ -> false
            )
          | [] -> false
        )
      | Tab_tok ->  
        (
          match tokens with
            y::ys ->
            (
              match y with
                Tab_tok -> indent_match ys xs
              | _ -> false
            )
          | [] -> false
        )
      | Space_tok ->
        (
          match tokens with
            y::ys ->
            (
              match y with
                Space_tok -> indent_match ys xs
              | _ -> false
            )
          | [] -> false
        )
      | _ ->
        (
          match tokens with
            y::ys ->
            (
              match y with
                Nl_tok -> false
              | Tab_tok -> false
              | Space_tok -> false
              | _ -> true
            )
          | [] -> true
        )  
    )
  | [] -> 
    (
      match tokens with
            y::ys ->
            (
              match y with
                Nl_tok -> raise IndentionError
              | Tab_tok -> raise IndentionError
              | Space_tok -> raise IndentionError
              | _ -> true
            )
          | [] -> true
    )
;;
(* this functions will create a block of statements to execute. 1 empty lists
   means we need to exit *)
let rec create_state_bl = fun acc -> fun indent_ -> fun name_list ->
  let _ = print_string "... " in
  let x = read_line () in
  if x = "exit()"
  then
   (acc, false)
  else
    let x = (Lexing.from_string x) in
    let x = unary 0 0 x in 
    if x <> [] then
      let indent_level = indent x [] in 
      if x = [] then create_state_bl acc indent_ name_list else 
      if is_else x then (acc, true)
      else
        let x = fix_x x [] in
        if x = [] then create_state_bl acc indent_ name_list
        else
        let assignment = check_assignment x in
        match assignment with
          (true,false) ->
          (
            let p = change_tokens x in
            let p = shunting_yard p [] [] name_list in
            let _ = post_order p in
            if indent_ = [] then
              if indent_level = [] then raise ExpectedBlock
              else
              if end_block acc x then (acc, false)
              else
                create_state_bl (acc@[x]) indent_level name_list
            else
            if end_block acc x then (acc, false)
            else
            if indent_match indent_level indent_
            then
              create_state_bl (acc@[x]) indent_ name_list
            else
              raise ExpectedBlock
          )
        | _ ->
          (
            let p = shunting_yard x [] [] name_list in
            let _ = post_order p in
            if indent_ = [] then
              if indent_level = [] then raise ExpectedBlock
              else
              if end_block acc x then (acc, false)
              else
                create_state_bl (acc@[x]) indent_level name_list
            else
            if end_block acc x then (acc, false)
            else
            if indent_match indent_level indent_
            then
              create_state_bl (acc@[x]) indent_ name_list
            else
              raise ExpectedBlock
          )
    else
    if indent_ = [] then
      raise ExpectedBlock
    else 
      if end_block acc x then (acc, false)
      else
        create_state_bl (acc@[x]) indent_ name_list
;;

(* the idea is to take the lexemes and use the shunting yard algorithm to build
   my parse tree. After this of course, I will use a traversal to evaluate the
   equation *)
let rec main = fun name_list ->
  let _ = print_string ">>> " in
  let x = read_line () in
  if x = "exit()"
  then
    ()
  else
    try
      let x = (Lexing.from_string x) in
      let x = unary 0 0 x in
      if x = [] then main name_list
      else
      if is_indented x then raise UnexpectedIndent
      else
        let x = fix_x x [] in
        let assignment = check_assignment x in
        match assignment with
          (true, false) ->
          (
            let var_name = get_var_name x in
            let updated_token_list = change_tokens x in
            let p = shunting_yard updated_token_list [] [] name_list in
            let p = post_order p in
            (* now we need to assign the value to our var_name but we need the
               right type, we will use a boolean flag *)
            match p with
              Int_tok a ->
              (
                main ((var_name, (Int_tok a)) :: name_list)
              )
            | Float_tok a ->
              (
                main ((var_name, (Float_tok a)) :: name_list)
              )
            | Bool_tok b ->
              (
                if b then
                  main ((var_name, (Bool_tok b)) :: name_list)
                else
                  main ((var_name, (Bool_tok b)) :: name_list)
              )
            | _ -> raise SyntaxError
          )
        | (false, true) ->
          (
            (* try to print variable name, if it doesn't exist then error *)
            match x with
              y::ys ->
              (
                match y with
                  Id_tok s ->
                  (
                    let _ = print_var name_list s in
                    main name_list
                  )
                | _ -> raise SyntaxError
              ) 
            | [] -> raise SyntaxError
          )
        | (false, false) ->
          (
            match x with
              y::ys ->
              (
                if is_print y
                then
                  let _ = search_print ys in
                  let p = shunting_yard ys [] [] name_list in
                  let p = post_order p in
                  let _ = print_token p in
                  main name_list
                else
                if is_if y
                then
                  let _ = search_if ys in
                  let bool = fix_if ys [] in
                  let bool = shunting_yard bool [] [] name_list in
                  let bool = post_order bool in
                  let block = create_state_bl [] [] name_list in
                  match block with
                    (a, false) ->
                    (
                      match bool with
                        Bool_tok b ->
                        (
                          if b then
                            let up_name_list = execute_block a name_list in
                            main up_name_list
                          else
                            main name_list
                        )
                      | _ -> raise SyntaxError
                    )
                  | (a, true) ->
                    (
                      let else_ = create_state_bl [] [] name_list in
                      match bool with
                        Bool_tok bool ->
                        (
                          match else_ with
                            (b, _) ->
                            (
                              if bool then
                                let up_name_list = execute_block a name_list in
                                main up_name_list
                              else
                                let up_name_list =
                                  execute_block b name_list in
                                main up_name_list
                            )
                          
                        )
                      | _ -> raise SyntaxError
                        
                    )
                else
                  let p = shunting_yard x [] [] name_list in
                  let p = post_order p in
                  let _ = print_token p in
                  main name_list
              )
            | [] -> raise SyntaxError
          )
        | (_, _) -> raise SyntaxError
    with
     
      SyntaxError -> let _ = e_s ()  in main name_list
    | IgnoreCase -> let _ = e_i () in main name_list
    | NotDefined -> let _ = e_d () in main name_list
    | Bad_Character -> let _ = e_c () in main name_list
    | DivisionByZero -> let _ = e_z () in main name_list
    | ExpectedBlock -> let _ = e_b () in main name_list
    | IndentionError -> let _ = e_l () in main name_list
    | UnexpectedIndent -> let _ = e_u () in main name_list
;;

main [];;

let x = (Lexing.from_string "+-+");;
let x = unary 0 x;;
fix_x x [];;
let y = shunting_yard x [] [] [];;
