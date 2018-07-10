module Storage = struct
  type t = {
    state: state;
    chars_map: chars;
  } and state = {
    base:  (int * int option) array;
    checked: int array;
  } and chars = {
    mutable char_index: int;
    mutable char_arr: char array;
    char_map: (char,int) Hashtbl.t;
  } 

  exception Conflict
  (*
    使用0代表空状态
    使用负数代表tails所在的位置
  *)
  let empty =
    let state = {
      base = [|(1,None)|];
      checked = [|0|];
    } in
    let chars_map = {
      char_index = 0;
      char_arr = Array.make 65535 @@ Char.chr(0);
      char_map = Hashtbl.create 65535;
    } in
    { state; chars_map}
  let state_size trie = 
    let state = trie.state in
    Array.length state.checked
 
  let state_offset ppos trie = 
    let state = trie.state in
    let (offset,_) = state.base.(ppos) in
    offset

  let is_empty_cell ppos trie = 
    let offset = state_offset ppos trie in
    offset == 0 
    

  let ensure_state pos trie =
    let state = trie.state in 
    let len = Array.length state.checked in 
    let expand size = 
      { trie with state = {
          base = Array.append state.base @@ Array.make size (0,None);
          checked = Array.append state.checked @@ Array.make size 0;
        };
      }
    in  
    if pos >= len 
    then expand (pos - len + 1)
    else trie

  let rec index_of_char chr trie = 
    let chars_map = trie.chars_map in 
    let modify_char_arr = fun index ->
      let len = Array.length chars_map.char_arr in 
      if index >= len 
        then chars_map.char_arr <- Array.append chars_map.char_arr @@ Array.make 65536 @@ Char.chr(0)
    in
    let insert_chars = fun _ -> 
      Hashtbl.add chars_map.char_map chr chars_map.char_index ;
      modify_char_arr chars_map.char_index ;
      Array.set chars_map.char_arr chars_map.char_index chr; 
      chars_map.char_index <- chars_map.char_index + 1;
    in
    let index = Hashtbl.find_opt chars_map.char_map chr  in 
    match index with 
      | Some x -> (x,trie)
      | None ->  
          insert_chars();
          index_of_char chr trie

  let add_transform pos ppos value trie =
    let nt = ensure_state pos trie in
    let offset = state_offset ppos nt in  
    let state = nt.state in
      Array.set state.checked pos (ppos + 1);
      Array.set state.base pos (offset,value);
      nt 

  let check_and_add_transform pos ppos value trie =
    let state = trie.state in 
    let checked = Array.get state.checked pos in 
    if checked == (ppos + 1)
    then begin 
      let offset = state_offset ppos trie in  
      Array.set state.base pos (offset,value);
      trie  
    end 
    else raise Conflict
  
  let rec next_alive_state ppos trie = 
    let len = state_size trie in 
    let n = ppos + 1 in
    if n >= len 
    then n
    else  if is_empty_cell n trie
          then n
          else next_alive_state n trie 

  let resolve_conflict index ppos vlaue trie = 
    let alive = next_alive_state ppos trie in 
    let new_offset = alive - index in 
     

  let add_char chr value ppos trie = 
    let (index,nt) = index_of_char chr trie in
    let len = state_size nt in 
    let offset = state_offset ppos nt in
    let t = offset + index in 
    if t >= len 
    then (t,add_transform t ppos value nt)
    else  try (t,check_and_add_transform t ppos value nt)
          with Conflict -> resolve_conflict index ppos value nt 
 


end
let empty = Storage.empty

let insert chrs value trie = 
  let len = (Array.length chrs) - 1 in 
  let fn = fun acc chr ->
    let (ppos,t,c) = acc in
    if len  == c 
    then  let (nppos,nt) = Storage.add_char chr (Some value) ppos t 
          in 
          (nppos,nt,c + 1)
    else  let (nppos,nt) = Storage.add_char chr None ppos t 
          in 
          (nppos,nt,c + 1)

  in 
  Array.fold_left fn (0,trie,0) chrs
