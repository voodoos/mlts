exception Query_failed
exception No_kernel

let kernel = ref None
                 
let compile code =
  try
    (* First mlts => lprolog *)
    let lpcode = Mlts_API.parse_and_translate code in

    (* updating the pseudo file *)
    Sys_js.update_file "core/progs_gen.mod" lpcode;

    (* recompiling lprolog code *)
    let parsed =
      Elpi_API.Parse.program
        ["core/progs_gen.mod";"core/run.mod"] in
    kernel := Some(Elpi_API.Compile.program [parsed]);

    (* We return the lprolog code for reference *)
    Js.string lpcode, 0, 0, true
  with Mlts_API.Error(s, line, char)
       -> (Js.string s, line, char,  false)
(*| _ -> Js.string "Unknown (probably parsing-related) error.", 0, 0, false*)

let query prog =
  (* First we check that the program have been compiled *)
    match !kernel with
      None -> raise No_kernel
    | Some(k) ->
       let goal = Elpi_API.Parse.goal prog in
       let goalc = Elpi_API.Compile.query k goal in
       match (Elpi_API.Execute.once k goalc) with
         Success(data) ->
          (* Elpi returns answers as a list of terms *)
          (* We transform it into a list of strings *)
          let resp = Array.map (fun term ->
                         Elpi_API.Pp.term
                           (Format.str_formatter)
                           term;
                         let str = Format.flush_str_formatter () in
                         (* LP strings are surrounded by quotes, we remove them *)
                         String.sub str 1 (String.length str - 2))
                               data.assignments in
          (* print_string ("res["^(resp.(0))^"]"); *)
          resp.(0)
            
       | _ -> raise Query_failed

let run () = query("run_all L")
                    
let compile_and_run code =
  let lpcode = compile (Js.to_string code) in
  lpcode, query ("run_all N.")
    
                 
let _ =
  (* Loading data folder in the pseudo-filesystem *)
  Data.load ();

  (* Initialize Elpi *)
  ignore (Elpi_API.Setup.init ~silent:true [] "");
  let parsed =  Elpi_API.Parse.program ["core/run.mod"] in
  kernel := Some(Elpi_API.Compile.program [parsed]);
  
  
  (* JS API *)
  Js.export "compile" (fun jstr -> compile (Js.to_string jstr)) ;
  Js.export "query"  (fun jstr -> query (Js.to_string jstr)) ;
  Js.export "run" run

                                                      

                                                    
                                     
