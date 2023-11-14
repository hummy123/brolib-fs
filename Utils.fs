module Brolib.Utils
open Brolib

let run_txns_result_tiny (arr : (int * int * string) array)  =
  Array.fold
    (fun rope (pos, del_num, ins_str) ->
      let rope = if del_num > 0 then Rope.delete pos del_num rope else rope in
      if ins_str <> "" then Rope.insert pos ins_str rope else rope)
    Rope.empty arr

let rec run_txns_acc counter arr acc =
  if counter = 1000 then acc
  else
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let _ = run_txns_result_tiny arr in
    let time = timer.ElapsedMilliseconds in
    (* Convert time from seconds to milliseconds. *)
    let time_diff = time.ToString() in
    run_txns_acc (counter + 1) arr (time_diff::acc)

open System.IO
let write_file file_name acc =
  let str = String.concat "," acc in
  File.WriteAllText(file_name, str)
