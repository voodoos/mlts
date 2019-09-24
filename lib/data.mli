(* Packed data for the js_of_ocaml pseudo-file system *)

type file = { name: string; text: string }

val files : file list

val load : unit -> unit
