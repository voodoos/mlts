     
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
     | _ -> Js.string "Unknown error.", 0, 0, false

let escape s =
  Js.to_string (Js.encodeURI (Js.string s))

let handle_out res iter f (out : Elpi_API.Execute.outcome) =
  match out with
  | Success(data) -> 
     
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
      
     res :=  "{ \"name\": \"" ^ (escape resp.(0)) ^ "\""
            ^ ", \"value\": \"" ^ (escape resp.(2)) ^ "\""
            ^ ", \"code\": \"" ^ (escape resp.(1)) ^ "\""
            ^ "}" ^ (if !iter > 0 then "," else (iter := 1; "")) ^ !res
  | _ -> ()

let query prog =
  (* First we check that the program have been compiled *)
  match !kernel with
    None -> raise No_kernel
  | Some(k) ->
     let goal = Elpi_API.Parse.goal prog in
     let goalc = Elpi_API.Compile.query k goal in
     let res = ref "] }" in
     let iter = ref 0 in
     Elpi_API.Execute.loop k goalc
                           (fun () -> true)
                           (handle_out res iter);
     "{ \"output\": [" ^ !res
                           

let run () = query("run_one Name Prog Value.")
                  
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

                                                      

                                                    
                                     
