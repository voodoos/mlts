     
exception Query_failed
exception No_kernel

let kernel = ref None
                 
let escape s =
  Js.to_string (Js.escape (Js.string s))
               
let console ?pref:(p = "") (str : string) =
  ignore (Js.Unsafe.eval_string ("sendLog(unescape('" ^ (escape (p ^ str)) ^"'))"))

let consoleError ?pref:(p = "") (str : string) =
  ignore (Js.Unsafe.eval_string ("sendLog(unescape('"
                                 ^ (escape ("<span style=\"color:red;\">"
                                            ^ p ^ str ^ "</span>"))
                                 ^ "'))"))
                 
let compile header code =
  try
    (* First mlts => lprolog *)
    let lpcode =
      Mlts_API.prologify code in

    (* updating the pseudo files *)
    Sys_js.update_file ~name:"core/progs_gen.mod" ~content:lpcode;
    (*Sys_js.update_file "core/datatypes.sig" typsig;
    Sys_js.update_file "core/datatypes.mod" typmod;*)

    (* recompiling lprolog code *)
    let parsed =
      Elpi_API.Parse.program
        ["core/datatypes.mod";
         "core/progs_gen.mod";
         "core/run.mod";] in
    kernel := Some(Elpi_API.Compile.program header [parsed]);

    (* We return the lprolog code for reference *)
    Js.string (lpcode)
              (* ^ "%%% Datatypes/sig." ^ typsig
               ^  "%%% Datatypes/mod." ^ typmod)*),
    Array.of_list [](* defs *) , 0, 0, true
  with Mlts_API.Error(s, line, char)
       -> (Js.string s, [||], line, char,  false)
  | _ -> Js.string "Unknown error.", [||], 0, 0, false


(* Names used in the query *)
let q_name  = "Name"
let q_prog  = "Prog"
let q_value = "Value"
let q_type  = "Type"

let handle_out res iter _f (out : Elpi_API.Execute.outcome) =
  match out with
  | Success(data) -> 
     
     (* Elpi returns answers as a map from query variable names to terms *)
     (* We transform it into a map from names to strings *)
     let resp =
       Elpi_API.Data.StrMap.map (fun term ->
         Elpi_API.Pp.term (Format.str_formatter) term;
         let str = Format.flush_str_formatter () in
         (* LP strings are surrounded by quotes, we remove them *)
         let str = String.sub str 1 (String.length str - 2) in
         escape str)
       data.assignments in
     let get name =
       try Elpi_API.Data.StrMap.find name resp
       with Not_found -> consoleError ("Assignment for " ^ name ^ " not found"); "error" in
      console ("<br> Finished " ^ get q_name ^ ".");
     flush_all ();
     res :=  "{ \"name\": \"" ^ get q_name ^ "\""
            ^ ", \"code\": \"" ^ get q_prog ^ "\""
            ^ ", \"value\": \"" ^ get q_value ^ "\""
            ^ ", \"type\": \"" ^ get q_type ^ "\""
            ^ "}" ^ (if !iter > 0 then "," else (iter := 1; "")) ^ !res
  | _ -> ()

let query prog =
  (* First we check that the program have been compiled *)
  match !kernel with
    None -> raise No_kernel
  | Some(k) ->
     let goal = Elpi_API.Parse.goal prog in
     let goalc = Elpi_API.Compile.query k goal in
     let exec = Elpi_API.Compile.link goalc in
     let res = ref "] }" in
     let iter = ref 0 in
     Elpi_API.Execute.loop exec
                           ~more:(fun () -> true)
                           ~pp:(handle_out res iter);
     
     flush_all ();
     "{ \"output\": [" ^ !res
                          

let run () = query Printf.(sprintf "run_one %s %s %s %s." q_name q_prog q_value q_type)
                  
let compile_and_run header code =
  let lpcode = compile header (Js.to_string code) in
  lpcode, query ("run_all N.")

let version = "0.1.12"

  
let _ =
  (* Redirect output to console *)
  Sys_js.set_channel_flusher stdout (console);
  Sys_js.set_channel_flusher stderr (consoleError);
  
  ignore (Js.Unsafe.eval_string ("sendVersion('" ^ (version) ^"')"));
  
  (* Loading data folder in the pseudo-filesystem *)
  Data.load ();

  (* Initialize Elpi *)
  let header, _ = Elpi_API.Setup.init ~silent:true [] ~basedir:"" ~builtins:Elpi_builtin.std_builtins in
  Elpi_API.Setup.set_warn (console ~pref:"[elpi]");
  Elpi_API.Setup.set_error (console ~pref:"[elpi]");
  Elpi_API.Setup.set_anomaly (console ~pref:"[elpi]");
  Elpi_API.Setup.set_type_error (console ~pref:"[elpi]");
  
  let parsed =  Elpi_API.Parse.program ["core/run.elpi"] in
  kernel := Some(Elpi_API.Compile.program header [parsed]);

  (* JS API *)
  Js.export "compile" (fun jstr -> compile header (Js.to_string jstr)) ;
  Js.export "query"  (fun jstr -> query (Js.to_string jstr)) ;
  Js.export "run" run                    

                                                    
                                     
 
