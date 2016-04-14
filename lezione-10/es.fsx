(* Lezione 10 *)
(*
 1. Definire mediante foldBack le seguenti funzioni 
    List.concat
    List.filter

    controllarne equivalenza con FsCheck
*)

(* 1.1 List.concat *)

printfn "list.concat: %A" (List.concat [[1;2;3]; [4;5;6]; [7;8;9]])

let mconcat xs = List.foldBack (fun x sl -> x :: xs) :: xs

//printfn "mconcat: %A" (mconcat [[1;2;3]; [4;5;6]; [7;8;9]])

(* 1.2 List.filter *)