type arr_trie = {
  mutable base: int array;
  mutable checked: int array;
  mutable tails: char array;
  mutable chars: char array;
}

let empty = {
  base = [|1|];
  checked = [||];
  tails = [||];
  chars = [||]
}

let pos_of_char chr trie = 
  let insert_chars = fun()->
      trie.chars <- Array.append trie.chars [| chr |];
      Array.length trie.chars
  in
  let i = ref (-1) in
  let () = Array.iteri (fun n elt -> if chr = elt then i := n else ()) trie.chars in
    if !i < 0 then 
      insert_chars() - 1
    else 
      !i
(*
  base[0] = 1 
  base[s] + c =t 
  check[t] = s， 即 check[ base[s] + c ] = s
*)