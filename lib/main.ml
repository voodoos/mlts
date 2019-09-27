open Js_of_ocaml
exception Query_failed
exception No_kernel

(* TryMLTS version *)
let version = "0.5"

(* Reference to the Elpi kernel *)
let kernel = ref None

(* Tools for printing to the browser's console *)
let escape s =
  Js.to_string (Js.escape (Js.string s))

let console ?pref:(p = "") (str : string) =
  ignore (Js.Unsafe.eval_string ("sendLog(unescape('" ^ (escape (p ^ str)) ^"'))"))

let consoleError ?pref:(p = "") (str : string) =
  ignore (Js.Unsafe.eval_string ("sendLog(unescape('"
                                 ^ (escape ("<span style=\"color:red;\">"
                                            ^ p ^ str ^ "</span>"))
                                 ^ "'))"))

let wrapConsole ?pref:(p = "") ?loc str = console str
let wrapConsoleErr ?pref:(p = "") ?loc str = consoleError str

(* Translate and compile MLTS code *)
let compile header code =
  try
    (* First mlts => lprolog *)
    let lpcode, defs =
      Mlts_API.prologify code in

    (* updating the pseudo files *)
    Sys_js.update_file ~name:"core/progs.elpi" ~content:lpcode;

    (* recompiling lprolog code *)
    let parsed =
      Elpi.API.Parse.program
        [
          "core/progs.elpi";
          "core/run.elpi";
        ] in
    kernel := Some(Elpi.API.Compile.program Elpi.API.Compile.default_flags header [parsed]);

    (* We return the lprolog code for reference *)
    Js.string (lpcode), Array.of_list defs, 0, 0, true

  with Mlts_API.Error(s, line, char)
    -> (Js.string s, [||], line, char,  false)
     | e -> Js.string ("Unexpected error: " ^ (Printexc.to_string e)), [||], 0, 0, false


(* Names used in the query *)
let q_name  = "Name"
let q_prog  = "Prog"
let q_value = "Value"
let q_type  = "Type"

(* Handler for Elpi output *)
let handle_out res iter _f (out : unit Elpi.API.Execute.outcome) =
  match out with
  | Success(data) ->

    (* Elpi returns answers as a map from query variable names to terms *)
    (* We transform it into a map from names to strings *)
    let resp =
      Elpi.API.Data.StrMap.map (fun term ->
          Elpi.API.Pp.term (data.pp_ctx) (Format.str_formatter) term;
          let str = Format.flush_str_formatter () in
          escape str)
        data.assignments
    in
    let get name =
      try Elpi.API.Data.StrMap.find name resp
      with Not_found -> consoleError ("Assignment for " ^ name ^ " not found"); "error"
    in
    console ("<br> Finished " ^ get q_name ^ ".");
    flush_all ();
    res :=  "{ \"name\": \"" ^ get q_name ^ "\""
            ^ ", \"code\": \"" ^ get q_prog ^ "\""
            ^ ", \"value\": \"" ^ get q_value ^ "\""
            ^ ", \"type\": \"" ^ get q_type ^ "\""
            ^ "}" ^ (if !iter > 0 then "," else (iter := 1; "")) ^ !res
  | _ -> flush_all ()

let query prog =
  (* First we check that the program have been compiled *)
  match !kernel with
    None -> raise No_kernel
  | Some(k) ->
    let goal = Elpi.API.Parse.goal (Elpi.API.Ast.Loc.initial "mlts") prog in
    let goalc = Elpi.API.Compile.query k goal in
    let exec = Elpi.API.Compile.link goalc in
    let res = ref "] }" in
    let iter = ref 0 in
    Elpi.API.Execute.loop exec
      ~more:(fun () -> true)
      ~pp:(handle_out res iter);

    flush_all ();
    "{ \"output\": [" ^ !res


let run () = query Printf.(sprintf "run %s %s %s %s." q_name q_prog q_value q_type)

let compile_and_run header code =
  let lpcode = compile header (Js.to_string code) in
  lpcode, query ("run_all N.")


let _ =
  (* Redirect output to console *)
  Sys_js.set_channel_flusher stdout (console);
  Sys_js.set_channel_flusher stderr (consoleError);

  ignore (Js.Unsafe.eval_string ("sendVersion('" ^ (version) ^"')"));

  (* Load data folder in the pseudo-filesystem *)
  Data.load ();

  (* Initialize Elpi *)
  let header, _ = Elpi.API.Setup.init [] ~basedir:"" ~builtins:Elpi.Builtin.std_builtins in
  Elpi.API.Setup.set_warn (wrapConsoleErr ~pref:"[elpi]");
  Elpi.API.Setup.set_error (wrapConsoleErr ~pref:"[elpi]");
  Elpi.API.Setup.set_anomaly (wrapConsole ~pref:"[elpi]");
  Elpi.API.Setup.set_type_error (wrapConsole ~pref:"[elpi]");

  let parsed =  Elpi.API.Parse.program ["core/run.elpi"] in
  kernel := Some(Elpi.API.Compile.program Elpi.API.Compile.default_flags header [parsed]);

  (* JS API *)
  Js.export "compile" (fun jstr -> compile header (Js.to_string jstr)) ;
  Js.export "query"  (fun jstr -> query (Js.to_string jstr)) ;
  Js.export "run" run




