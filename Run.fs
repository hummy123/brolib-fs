module Brolib.Run

[<EntryPoint>]
let main _ =
  let svelte = Utils.run_txns_acc 0 Svelte.data [] in
  let rust = Utils.run_txns_acc 0 Rust.data [] in
  let seph = Utils.run_txns_acc 0 Seph.data [] in
  let automerge = Utils.run_txns_acc 0 Automerge.data [] in

  let _ = Utils.write_file "fs_svelte.csv" svelte in
  let _ = Utils.write_file "fs_rust.csv" rust in
  let _ = Utils.write_file "fs_seph.csv" seph in
  let _ = Utils.write_file "fs_automerge.csv" automerge in

  0
