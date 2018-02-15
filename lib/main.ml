exception Query_failed
exception No_kernel

let kernel = ref None 

let compile code =
  let lpcode = Mlts_API.parse_and_translate code in
  print_string lpcode;
  Sys_js.update_file "core/progs_gen.mod" lpcode;
  let parsed =  Elpi_API.Parse.program ["core/run.mod"] in
  kernel := Some(Elpi_API.Compile.program [parsed]);
  lpcode

let query prog =

    match !kernel with
      None -> raise No_kernel
    | Some(k) ->
       let goal = Elpi_API.Parse.goal prog in
       let goalc = Elpi_API.Compile.query k goal in
       match (Elpi_API.Execute.once k goalc) with
         Success(data) ->
          let assignments = data.assignments in
          
          let resp = Array.map (fun term -> Elpi_API.Pp.term
                                              (Format.str_formatter)
                                              term;
                                            (Format.flush_str_formatter ()))
                               assignments in
          (Array.fold_left
             (fun acc s -> acc ^ s) "" resp)
            
       | _ -> raise Query_failed

let run () = query("run_all L")
                    
let compile_and_run code =
  let lpcode = compile (Js.to_string code) in
  lpcode, query ("run_all N.")
    
(* JS API *)
                 
let _ =
  (* Loading data folder in the pseudo-filesystem *)
  Data.load ();

  (* Initialize Elpi *)
  ignore (Elpi_API.Setup.init [] "");
(*
  compile "3 + 4;;";
  
  print_string (query ("run_all N."));*)
  
  Js.export "compile" (fun jstr -> compile (Js.to_string jstr)) ;
  Js.export "run" run

                                                      

                                                    
                                     
