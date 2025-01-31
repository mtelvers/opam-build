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
  fun v -> match (String.split_on_char ':' (OpamVariable.Full.to_string v)) with
    | [ "make" ] -> Some (OpamVariable.string "make")
    | [ "name" ] -> Some (OpamVariable.string name)
    | [ "jobs" ] -> Some (OpamVariable.int num_cores)
    | [ "dev" ] -> Some (OpamVariable.bool dev)
    | [ "with-test" ]
    | [ "with-doc" ] -> Some (OpamVariable.bool false)
    | [ "prefix" ] -> Some (OpamVariable.string "/home/opam/.opam/5.3")
    | _ :: [ "bin" ] -> Some (OpamVariable.string "/home/opam/.opam/5.3/bin")
    | _ :: [ "lib" ] -> Some (OpamVariable.string "/home/opam/.opam/5.3/lib")
    | _ :: [ "man" ] -> Some (OpamVariable.string "/home/opam/.opam/5.3/man")
    | _ :: [ "doc" ] -> Some (OpamVariable.string "/home/opam/.opam/5.3/doc")
    | [ "ocaml"; "version" ] -> Some (OpamVariable.string "5.3.0")
    | [ "ocaml"; "preinstalled" ] -> Some (OpamVariable.bool false)
    | [ "ocaml"; "native" ] -> Some (OpamVariable.bool true)
    | [ "pinned" ] -> Some (OpamVariable.bool true)
    | x -> List.iter (Printf.fprintf stderr "Missing var %s\n") x ; None

let env =
  fun v -> match (OpamVariable.Full.to_string v) with
  | "os" -> Some (OpamVariable.string "linux")
  | "os-family" -> Some (OpamVariable.string "debian")
  | "os-distribution" -> Some (OpamVariable.string "ubuntu")
  | "os-version" -> Some (OpamVariable.string "24.10")
  | x -> Printf.printf "env %s\n" x;
         Some (OpamVariable.string x)

let filename =
  Arg.(value & pos 0 non_dir_file "opam" &
       info ~docv:"OPAM-FILE" ~doc:"Path to the opam file." [])

let project_name =
  Arg.(value & (opt string "name") &
       info ~docv:"PROJECT-NAME" ~doc:"The name of the package." ["name"])

let dev =
  Arg.(value & flag &
       info ~docv:"DEV-CMD" ~doc:"Output dev commands as well." ["dev"])

let num_cores =
  Arg.(value & opt int 1 &
       info ~docv:"NUM" ~doc:"Number of cores to pass to dune." ["j"; "num-cores"])

let depext =
  Arg.(value & flag &
       info ~docv:"DEPEXT-CMD" ~doc:"Output external dependencies as well." ["depext"])

let exec =
  Arg.(value & flag &
       info ~docv:"EXEC" ~doc:"Run the commands." ["exec"])

let build filename name dev num_cores depext exec =
  let opam = read_opam_file filename in
  let rc =
    if depext then
      OpamFile.OPAM.depexts opam |> List.fold_left (fun acc (names, filter) ->
        if OpamFilter.eval_to_bool env filter then
          OpamSysPkg.Set.fold (fun pkg acc ->
            let s = OpamSysPkg.to_string pkg in
            Printf.printf "%s\n" s;
            if acc = 0 && exec then
              Sys.command (Filename.quote_command "apt" ["install"; s])
            else acc
          ) names acc
        else acc
      ) 0
    else 0 in
  OpamSystem.init ();
  OpamCoreConfig.init ();
  OpamFormatConfig.init ();
  OpamRepositoryConfig.init ();
  OpamClientConfig.init ();
  OpamStateConfig.init ();
  let _ = OpamPath.init_config_files ();
  let gt = OpamGlobalState.load `Lock_none in
  let switches = OpamGlobalState.switches gt in
  let () = List.iteri (fun i sw -> Printf.printf "opam switch %i: %s\n" (i + 1) (OpamSwitch.to_string sw)) switches in
          (*
          let root_from, root_dir = OpamStateConfig.opamroot_with_provenance () in
          OpamStateConfig.init ~root_from ~root_dir ();
  let switch = OpamStateConfig.get_current_switch_from_cwd (OpamFilename.cwd ()) in
  *)

          (*
  let switch = OpamStateConfig.get_switch_opt () in
  let _ = match switch with
  | None -> Printf.printf "No switch....\n"
  | Some switch -> (
  *)
  let switch = List.hd switches in
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_write gt ~switch (fun st ->
          (*
          (OpamClient.install ~ignore_conflicts:true st [(OpamPackage.Name.of_string "zlib", Some (`Eq, OpamPackage.Version.of_string "0.8"))])
          OpamSwitchState.drop @@
  *)
          (
    let source_dir nv =
      let opam = OpamSwitchState.opam st nv in
      let raw = OpamSwitchState.source_dir st nv in
      match OpamFile.OPAM.url opam with
      | None -> raw
      | Some url -> OpamFilename.SubPath.(raw /? OpamFile.URL.subpath url)
     in
     let pkg = OpamPackage.of_string "z3.4.13.3" in
     let src = source_dir pkg in
     OpamAction.download_package st pkg |> OpamProcess.Job.run |> function
                  | Some exn -> Printf.printf "download failed...\n"
                  | None -> Printf.printf "download success...\n" ;
OpamAction.prepare_package_source st pkg src |> OpamProcess.Job.run |> function
                  | Some exn -> Printf.printf "prepare failed...\n"
                  | None -> Printf.printf "prepare success...\n" ;
      OpamAction.build_package st src pkg |> OpamProcess.Job.run |> function
                  | Some exn -> Printf.printf "build failed...\n"
                  | None -> Printf.printf "build success...\n";
      OpamAction.install_package st ~build_dir:src pkg |> OpamProcess.Job.run |> function
                  | Right exn -> Printf.printf "install failed...\n"
                  | Left conf ->
                                  Printf.printf "install success...\n";
                                  ignore (OpamSwitchAction.add_to_installed st pkg)
      )
          ) in
  let cmds = OpamFilter.commands (filter ~name ~dev ~num_cores) (OpamFile.OPAM.build opam @ OpamFile.OPAM.install opam) in
  List.fold_left (fun acc cmd ->
    List.iter (Printf.printf "%s ") cmd; Printf.printf "\n";
    if acc = 0 && exec then
      Sys.command (Filename.quote_command (List.hd cmd) (List.tl cmd))
    else acc) rc cmds

let cmd =
  let doc = "print opam build commands one per line" in
  let info = Cmd.info "opam-build" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const build $ filename $ project_name $ dev $ num_cores $ depext $ exec)

let () = exit (Cmd.eval' cmd)
