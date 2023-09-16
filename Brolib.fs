module Brolib.Rope

type rope =
  | N0 of string
  | N1 of rope
  | N2 of rope * int * int * rope

  | L2 of string * string
  | N3 of rope * rope * rope

[<Literal>]
let target_length = 1024

(* An empty Rope. *)
let empty = N0 ""

(* A new Rope, initialised with the provided string. *)
let of_string string = N0 string

let rec size = function
  | N0 s -> String.length s
  | N1 t -> size t
  | N2(_, lm, rm, _) -> lm + rm
  | N3(t1, t2, t3) -> size t1 + size t2 + size t3
  | _ -> failwith "impossible case"

let root = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, String.length s2, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 
      let t2_size = size t2
      let left = N2 (t1, t1_size, t2_size, t2) 
      N2 (left, t1_size + t2_size, size t3, N1 t3)
  | t -> t

let n1 = function
  | L2 (s1, s2) -> N2 (N0 s1, String.length s1, String.length s2, N0 s2)
  | N3 (t1, t2, t3) ->
      let t1_size = size t1 
      let t2_size = size t2
      let left = N2 (t1, t1_size, t2_size, t2) 
      N2 (left, t1_size + t2_size, size t3, N1 t3)
  | t -> N1 t

let ins_n2_left left right =
  match (left, right) with
  | L2 (s1, s2), t3 -> N3 (N0 s1, N0 s2, t3)
  | N3 (t1, t2, t3), N1 t4 ->
      let t1_size = size t1 
      let t2_size = size t2
      let left = N2 (t1, t1_size, t2_size, t2) 
      let t3_size = size t3 
      let t4_size = size t4
      let right = N2 (t3, t3_size, t4_size, t4) 
      N2 (left, t1_size + t2_size, t3_size + t4_size, right)
  | N3 (t1, t2, t3), (N2 _ as t4) ->
      N3 (N2 (t1, size t1, size t2, t2), N1 t3, t4)
  | N3 (t1, t2, t3), t4 ->
      let t1_size = size t1 
      let t2_size = size t2
      let left = N2 (t1, t1_size, t2_size, t2) 
      let t3_size = size t3
      let t4_size = size t4
      let right = N2 (t3, t3_size, t4_size, t4)
      N2 (left, t1_size + t2_size, t3_size + t4_size, right)
  | l, r -> N2 (l, size l, size r, r)

let ins_n2_right left right =
  match (left, right) with
  | t1, L2 (s1, s2) -> N3 (t1, N0 s1, N0 s2)
  | N1 t1, N3 (t2, t3, t4) ->
      let t1_size = size t1
      let t2_size = size t2
      let left = N2 (t1, t1_size, t2_size, t2)
      let t3_size = size t3
      let t4_size = size t4
      let right = N2 (t3, t3_size, t4_size, t4)
      N2 (left, t1_size + t2_size, t3_size + t4_size, right)
  | (N2 _ as t1), N3 (t2, t3, t4) ->
      N3 (t1, N1 t2, N2 (t3, size t3, size t4, t4))
  | t1, N3 (t2, t3, t4) ->
      let t1_size = size t1
      let t2_size = size t2
      let left = N2 (t1, t1_size, t2_size, t2)
      let t3_size = size t3
      let t4_size = size t4
      let right = N2 (t3, t3_size, t4_size, t4)
      N2 (left, t1_size + t2_size, t3_size + t4_size, right)
  | l, r -> N2 (l, size l, size r, r)

let rec ins cur_index string = function
  | N0 str ->
      if cur_index <= 0 then
        if String.length str + String.length string <= target_length then
          N0 (string + str)
        else L2 (string, str)
      else if cur_index >= String.length str then
        if String.length str + String.length string <= target_length then
          N0 (str + string)
        else L2 (str, string)
      else
        let sub1 = str[0..cur_index] in
        let sub2 = str[cur_index..(String.length str - cur_index)] in
        if String.length str + String.length string <= target_length then
          N0 (sub1 + string + sub2)
        else if String.length sub1 + String.length string <= target_length then
          L2 (sub1 + string, sub2)
        else if String.length sub2 + String.length string <= target_length then
          L2 (sub1, string + sub2)
        else
          (* String must be split into 3 different parts. *)
          N3 (N0 sub1, N0 string, N0 sub2)
  | N1 t -> n1 (ins cur_index string t)
  | N2 (l, lm, _, r) ->
      if cur_index < lm then ins_n2_left (ins cur_index string l) r
      else ins_n2_right l (ins (cur_index - lm) string r)
  | _ -> failwith ""

(* 
  Inserts a string into the Rope at the specified index.
  No errors are given at all.
  If the index is less than 0, the string is inserted to the start of the Rope.
  Is the index is more than the rope's length, the string is inserted to the end of the Rope.
*)
let insert index string rope = root (ins index string rope)

let rec prepend_internal string = function
  | N0 str ->
      if String.length str + String.length string <= target_length then
        N0 (string + str)
      else L2 (string, str)
  | N1 t -> n1 (prepend_internal string t)
  | N2 (l, _, _, r) -> ins_n2_left (prepend_internal string l) r
  | _ -> failwith ""

(* Prepends a string to the start of a Rope. *)
let prepend string rope = root (prepend_internal string rope)

let rec append_internal string = function
  | N0 str ->
      if String.length str + String.length string <= target_length then
        N0 (str + string)
      else L2 (str, string)
  | N1 t -> n1 (append_internal string t)
  | N2 (l, _, _, r) -> ins_n2_right l (append_internal string r)
  | _ -> failwith ""

(* Appends a string to the end of a Rope. *)
let append string rope = root (append_internal string rope)

let rec substring_internal start_idx end_idx acc = function
  | N0 str ->
      if start_idx <= 0 && end_idx >= String.length str then
        (* In range. *)
        str :: acc
      else if start_idx >= 0 && end_idx <= String.length str then
        (* In middle of this node. *)
        let str = str[start_idx..(end_idx - start_idx)] in
        str :: acc
      else if start_idx >= 0 && end_idx >= String.length str then
        (* Starts at this node. *)
        let str = str[start_idx..(String.length str - start_idx)] in
        str :: acc
      else
        (* Ends at this node. *)
        let str = str[0..end_idx] in
        str :: acc
  | N1 t -> substring_internal start_idx end_idx acc t
  | N2 (l, lm, _, r) ->
      (* Cases we need to consider.
         1. start_idx and end_idx are in same directions (both less than or both greater than weight).
         2. start_idx and end_idx are in different direction (start_idx is less than weight while end_idx is less than weight.)
      *)
      if lm > start_idx && lm > end_idx then
        substring_internal start_idx end_idx acc l
      else if lm < start_idx && lm < end_idx then
        substring_internal (start_idx - lm) (end_idx - lm) acc r
      else
        let acc = substring_internal (start_idx - lm) (end_idx - lm) acc r in
        substring_internal start_idx end_idx acc l
  | _ -> failwith ""

(*
  Returns a substring from the Rope.
  No errors are given at all; if start + length exceeds the Rope's length, 
  then the substring returned is up to the end of the Rope.
  If start is less than 0, but start + length is more than 0, 
  then the resulting substring is from [0..(start + length)].

  Potential optimisation opportunity:
  This works internally by coonsing (::) a list of strings from right to left
  (the same order used by Set/Map.foldBack),
  and then using the String.concat function.

  This means, first traversing the rope, and then traversing the linked list we build after that.

  I expect that a faster method would be to create a StringBuilder as long as the length parameter,
  and to append to the StringBuilder each time instead of consing :: a list of strings.
  Becase then we wouldn't need to traverse a linked list too.
  However, I didn't get any speed improvements when I tried the equivalent in OCaml 
  which is very strange and unexpected.
*)
let substring start length rope =
  substring_internal start (start + length) [] rope |> String.concat ""

let rec del_internal start_idx end_idx = function
  | N0 str ->
      if start_idx <= 0 && end_idx >= String.length str then
        (* In range. *)
        (N0 "", false)
      else if start_idx >= 0 && end_idx <= String.length str then
        (* In middle of this node. *)
        let sub1 = str[0..start_idx] in
        let sub2 = str[end_idx..(String.length str - end_idx)] in
        if String.length sub1 + String.length sub2 <= target_length then
          (N0 (sub1 + sub2), false)
        else (L2 (sub1, sub2), true)
      else if start_idx >= 0 && end_idx >= String.length str then
        (* Starts at this node. *)
        let str = str[0..start_idx] in
        (N0 str, false)
      else
        (* Ends at this node. *)
        let str = str[end_idx..(String.length str - end_idx)] in
        (N0 str, false)
  | N1 t ->
      let t, did_ins = del_internal start_idx end_idx t in
      if did_ins then (n1 t, true) else (N1 t, false)
  | N2 (l, lm, rm, r) ->
      if lm > start_idx && lm > end_idx then
        let l, did_ins = del_internal start_idx end_idx l in
        match did_ins with
        | false -> (N2 (l, size l, rm, r), false)
        | true -> (ins_n2_left l r, true)
      else if lm < start_idx && lm < end_idx then
        let r, did_ins = del_internal (start_idx - lm) (end_idx - lm) r in
        match did_ins with
        | false -> (N2 (l, lm, size r, r), false)
        | true -> (ins_n2_right l r, true)
      else
        (* It is only possible for did_ins to be true for one side as it only happens when deleting at the middle of a node. *)
        let r, did_ins_r = del_internal (start_idx - lm) (end_idx - lm) r in
        let l, did_ins_l = del_internal start_idx end_idx l in
        if did_ins_l then (ins_n2_left l r, true)
        else if did_ins_r then (ins_n2_right l r, true)
        else (N2 (l, size l, size r, r), false)
  | _ -> failwith ""

(*
  Removes the characters in the range specified by start (index) and length within the Rope.
  No errors are given at all.
  If the length given as an argument exceeds the length contained in the rope, 
  this function deletes from the end as much as it can.
  If the start given as an argument is less than 0 and start + length is less than 0, nothing is changed.
  If start is less than 0, but start + length is more than 0, then the range [0..(start + length)] is deleted.
*)
let delete start length rope =
  let rope, did_ins = del_internal start (start + length) rope in
  if did_ins then root rope else rope

let rec fold f state = function
  | N0 "" -> state
  | N0 str -> f state str
  | N1 t -> fold f state t
  | N2 (l, _, _, r) ->
      let state = fold f state l in
      fold f state r
  | _ -> failwith ""

open System.Text

(* Returns all the text contained in the Rope as a string. *)
let to_string rope =
  let sb = new StringBuilder(size rope) in
  fold (fun _ str -> sb.Append(str) |> ignore) () rope
  sb.ToString()

(* Public method just like CharRope.IndexOf in AvalonEdit. *)
let index_of string (search_text: string) start_index length (compare_type : System.StringComparison) rope =
  let str = substring start_index length rope in
  str.IndexOf(search_text, compare_type)

(* Public method just like CharRope.LastIndexOf in AvalonEdit. *)
let last_index_of string (search_text: string) start_index length (compare_type : System.StringComparison) rope =
  let str = substring start_index length rope in
  str.LastIndexOf(search_text, compare_type)

(* Public method just like CharRope.IndexOfAny in AvalonEdit. *)
let index_of_any any_of start_index length rope =
  let rec check_each_char_in_array pos search_chr =
    if pos = Array.length any_of then
      false
    else
      let any_of_chr = any_of[pos] in
      if any_of_chr = search_chr then true
      else check_each_char_in_array (pos + 1) search_chr
  in
  let rec check_each_char_in_substring pos string =
    if pos = String.length string then false
    else if check_each_char_in_array 0 string[pos] then
      true
    else
      check_each_char_in_substring (pos + 1) string
  in
  let substring = substring start_index length rope in
  check_each_char_in_substring 0 substring
