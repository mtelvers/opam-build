open Cmdliner

let read_opam_file file =
  let txt =
    try
      let input = In_channel.with_open_text file @@ fun ic -> In_channel.input_lines ic in
      String.concat "\n" input
    with e -> raise e
  in
  OpamFile.OPAM.read_from_string txt

let filter ~name ~dev ~num_cores =
  fun v -> match (OpamVariable.Full.to_string v) with
    | "name" -> Some (OpamVariable.string name)
    | "jobs" -> Some (OpamVariable.int num_cores)
    | "dev" -> Some (OpamVariable.bool dev)
    | "with-test"
    | "with-doc" -> Some (OpamVariable.bool false)
    | _ -> None

let filename =
  Arg.(value & pos 0 non_dir_file "opam" &
       info ~docv:"OPAM-FILE" ~doc:"Path to the opam file." [])

let project_name =
  Arg.(value & (opt string "name") &
       info ~docv:"PROJECT-NAME" ~doc:"Path to the opam file." ["name"])

let dev =
  Arg.(value & flag &
       info ~docv:"DEV-CMD" ~doc:"Output dev commands as well." ["dev"])

let num_cores =
  Arg.(value & opt int 1 &
       info ~docv:"NUM" ~doc:"Number of cores to pass to dune." ["j"; "num-cores"])

let exec =
  Arg.(value & flag &
       info ~docv:"EXEC" ~doc:"Run the commands." ["exec"])

let build filename name dev num_cores exec =
  let opam = read_opam_file filename in
  let cmds = OpamFilter.commands (filter ~name ~dev ~num_cores) (OpamFile.OPAM.build opam) in
  List.fold_left (fun acc cmd ->
    List.iter (Printf.printf "%s ") cmd; Printf.printf "\n";
    if acc = 0 && exec
    then Sys.command (Filename.quote_command (List.hd cmd) (List.tl cmd))
    else acc) 0 cmds

let cmd =
  let doc = "print opam build commands one per line" in
  let info = Cmd.info "opam-build" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const build $ filename $ project_name $ dev $ num_cores $ exec)

let () = exit (Cmd.eval' cmd)
