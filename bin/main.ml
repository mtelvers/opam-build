open Cmdliner

let build name =
  OpamSystem.init ();
  let yes = Some (Some true) in
  let confirm_level = Some `unsafe_yes in
  OpamCoreConfig.init ?yes ?confirm_level ();
  OpamFormatConfig.init ();
  OpamClientConfig.init ();
  (*
  OpamRepositoryConfig.init ();
  OpamStateConfig.init ();
  OpamArg.init_opam_env_variabes OpamCLIVersion.Sourced.current;
  *)
  let gt = OpamGlobalState.load `Lock_none in
  let switches = OpamGlobalState.switches gt in
  (* I'd rather do this: let switch = OpamStateConfig.get_current_switch_from_cwd (OpamFilename.cwd ()) in *)
  let switch = List.hd switches in
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_write gt ~switch (fun st ->
      let source_dir nv =
        let opam = OpamSwitchState.opam st nv in
        let raw = OpamSwitchState.source_dir st nv in
        match OpamFile.OPAM.url opam with
        | None -> raw
        | Some url -> OpamFilename.SubPath.(raw /? OpamFile.URL.subpath url)
      in
      let pkg = OpamPackage.of_string name in
      let src = source_dir pkg in
      ignore (OpamSolution.install_depexts ~confirm:false st (OpamPackage.Set.singleton pkg));
      OpamAction.download_package st pkg |> OpamProcess.Job.run |> function
      | Some (_, e) ->
          Printf.printf "download failed... %s\n" e;
          1
      | None -> (
          OpamAction.prepare_package_source st pkg src |> OpamProcess.Job.run |> function
          | Some exn ->
              Printf.printf "prepare failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
              1
          | None -> (
              OpamAction.build_package st src pkg |> OpamProcess.Job.run |> function
              | Some exn ->
                  Printf.printf "build failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
                  1
              | None -> (
                  OpamAction.install_package st ~build_dir:src pkg |> OpamProcess.Job.run |> function
                  | Right exn ->
                      Printf.printf "install failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
                      1
                  | Left _ ->
                      ignore (OpamSwitchAction.add_to_installed st pkg);
                      0))))

let package_name = Arg.(value & pos 0 string "" & info ~docv:"PACKAGE-NAME" ~doc:"The name and version of the package in the format pkg.x.y.z." [])

let cmd =
  let doc = "Installs an opam package in the default switch. No checks, no questions asked." in
  let info = Cmd.info "opam-build" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const build $ package_name)

let () = exit (Cmd.eval' cmd)
