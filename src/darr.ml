type arr_trie = {
  base: int array;
  checked: int array;
  tails: char array;
  chars: char array;

}

let empty = {
  base = [|0 ; 1|];
  checked = [|0 ; 0|];
  tails = [||];
  chars = [||]
}

let pos_of_char chr trie = 
  let insert_chars = fun()->
      let new_trie = {trie with chars = Array.append trie.chars [| chr |];}
      in 
      (Array.length new_trie.chars,new_trie)
  in
  let i = ref (-1) in
  let () = Array.iteri (fun n elt -> if chr = elt then i := n else ()) trie.chars in
    if !i < 0 then 
      let (len,new_trie) = insert_chars()
      in 
      (len,new_trie)
    else 
      (!i + 1,trie)
(*
  base[1] = 1 
  base[s] + c = t 
  check[t] = s， 即 check[ base[s] + c ] = s
*)
let append_trie size trie = 
  {trie with 
    base = Array.append trie.base @@ Array.make size 0;
    checked = Array.append trie.checked @@ Array.make size 0;
    
  }
let insert_char parent chr trie  = 
  let (pos,nt) = pos_of_char chr trie in 
  let len = Array.length nt.base in
  let s = nt.base.(parent) in 
  let t = s + pos in 
  let ensure_space = fun() ->
    if t >= len then 
      append_trie (t - len + 1) nt
    else
      nt
  in 
  let new_trie = ensure_space() in
  let check = new_trie.checked.(t) in 
  let update_trie = fun()->
      Array.set new_trie.checked t parent;
      Array.set new_trie.base t s;
      new_trie
  in
    if check == 0 then 
      update_trie()
    else 
      new_trie
