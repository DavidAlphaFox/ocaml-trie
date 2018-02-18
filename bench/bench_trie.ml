open Core
open Core_bench

let naive_search ~max_differences key =
  List.filter ~f:(fun candidate ->
    Levenshtein.String.distance
      ~upper_bound:max_differences candidate key <= max_differences)

let () =
  let strings =
    let state = Caml.Random.State.make_self_init () in
    let string_gen = QCheck.small_printable_string in
    List.init 1000 ~f:(fun _ -> string_gen.gen state)
  in
  let kv_list = List.map strings ~f:(fun key -> key, ()) in
  let map = String.Map.of_alist_multi kv_list in
  let hash_table = String.Table.of_alist_multi kv_list in
  let trie = List.fold kv_list ~init:Char_trie.empty
      ~f:(fun acc (key, value) ->
        let key = String.to_list key in
        Char_trie.add key value acc)
  in
  let fast_trie = List.fold kv_list ~init:Fast_char_trie.empty
      ~f:(fun acc (key, value) ->
        let key = String.to_list key in
        Fast_char_trie.add key value acc)
  in
  let list_trie = List.fold kv_list ~init:Char_list_trie.empty
      ~f:(fun acc (key, value) ->
        let key = String.to_list key in
        Char_list_trie.add key value acc)
  in
  [ Bench.Test.create ~name:"populate map"
      (fun () ->
        String.Map.of_alist_multi kv_list)
  ; Bench.Test.create ~name:"populate hash table"
      (fun () ->
        String.Table.of_alist_multi kv_list)
  ; Bench.Test.create ~name:"populate trie"
    (fun () ->
      List.fold kv_list ~init:Char_trie.empty ~f:(fun acc (key, value) ->
        let key = String.to_list key in
        Char_trie.add key value acc))
  ; Bench.Test.create ~name:"populate fast trie"
    (fun () ->
      List.fold kv_list ~init:Fast_char_trie.empty ~f:(fun acc (key, value) ->
        let key = String.to_list key in
        Fast_char_trie.add key value acc))
  ; Bench.Test.create ~name:"populate list trie"
    (fun () ->
      List.fold kv_list ~init:Fast_char_trie.empty ~f:(fun acc (key, value) ->
        let key = String.to_list key in
        Fast_char_trie.add key value acc))
  ; Bench.Test.create ~name:"lookup keys map"
    (fun () ->
      List.map strings ~f:(Map.find map))
  ; Bench.Test.create ~name:"lookup keys hash table"
    (fun () ->
      List.map strings ~f:(String.Table.find hash_table))
  ; Bench.Test.create ~name:"lookup keys trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_trie.find key trie))
  ; Bench.Test.create ~name:"lookup keys fast trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Fast_char_trie.find key fast_trie))
  ; Bench.Test.create ~name:"lookup keys list trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Fast_char_trie.find key fast_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:0 trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_trie.find_approximate ~max_differences:0 key trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:1 trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_trie.find_approximate ~max_differences:1 key trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:2 trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_trie.find_approximate ~max_differences:2 key trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:3 trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_trie.find_approximate ~max_differences:3 key trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:0 fast trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Fast_char_trie.find_approximate ~max_differences:0 key fast_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:1 fast trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Fast_char_trie.find_approximate ~max_differences:1 key fast_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:2 fast trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Fast_char_trie.find_approximate ~max_differences:2 key fast_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:3 fast trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Fast_char_trie.find_approximate ~max_differences:3 key fast_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:0 list trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_list_trie.find_approximate ~max_differences:0 key list_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:1 list trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_list_trie.find_approximate ~max_differences:1 key list_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:2 list trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_list_trie.find_approximate ~max_differences:2 key list_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:3 list trie"
    (fun () ->
      List.map strings ~f:(fun key ->
        let key = String.to_list key in
        Char_list_trie.find_approximate ~max_differences:3 key list_trie))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:0 naive"
    (fun () ->
      List.map strings ~f:(fun key ->
        naive_search ~max_differences:0 key strings))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:1 naive"
    (fun () ->
      List.map strings ~f:(fun key ->
        naive_search ~max_differences:1 key strings))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:2 naive"
    (fun () ->
      List.map strings ~f:(fun key ->
        naive_search ~max_differences:2 key strings))
  ; Bench.Test.create ~name:"find_approximate ~max_differences:3 naive"
    (fun () ->
      List.map strings ~f:(fun key ->
        naive_search ~max_differences:3 key strings)) ]
  |> Bench.make_command
  |> Command.run
