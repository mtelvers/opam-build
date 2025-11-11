open Cmdliner

let build verbose_level debug_level name test doc dev_setup =
  OpamSystem.init ();
  let yes = Some (Some true) in
  let confirm_level = Some `unsafe_yes in
  let verbose_level = if List.is_empty verbose_level then None else Some (List.length verbose_level) in
  let debug_level = if List.is_empty debug_level then None else Some (List.length debug_level) in
  OpamCoreConfig.init ?yes ?confirm_level ?verbose_level ?debug_level ();
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
      let build_dir = OpamPath.Switch.build_dir st.switch_global.root st.switch in
      let build_dir = OpamFilename.SubPath.(build_dir / of_string name) in
      ignore (OpamSolution.install_depexts ~confirm:false ~pkg_to_install:(OpamPackage.Set.singleton pkg) ~pkg_installed:st.installed st);
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
              OpamFilename.copy_dir ~src ~dst:build_dir;
              OpamAction.build_package st ~test ~doc ~dev_setup build_dir pkg |> OpamProcess.Job.run |> function
              | Some exn ->
                  Printf.printf "build failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
                  1
              | None -> (
                  OpamAction.install_package st ~test ~doc ~dev_setup ~build_dir pkg |> OpamProcess.Job.run |> function
                  | Right exn ->
                      Printf.printf "install failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
                      1
                  | Left conf ->
                      let conf_files =
                        let add_conf conf = OpamPackage.Name.Map.add pkg.name conf st.conf_files in
                        OpamStd.Option.map_default add_conf st.conf_files conf in
                      ignore (OpamSwitchAction.add_to_installed { st with conf_files } pkg);
                      0))))

let package_name = Arg.(value & pos 0 string "" & info ~docv:"PACKAGE-NAME" ~doc:"The name and version of the package in the format pkg.x.y.z." [])

let verbose_level = Arg.(value & flag_all & info [ "v"; "verbose" ] ~docv:"VERBOSELEVEL" ~doc:"Increase the log level. Use several times to increase, e.g. '-vv'")

let debug_level = Arg.(value & flag_all & info [ "d"; "debug" ] ~docv:"DEBUGLEVEL" ~doc:"Increase the debug level. Use several times to increase, e.g. '-dd'")

let with_test = Arg.(value & flag & info [ "t"; "with-test" ] ~docv:"WITH-TEST" ~doc:"Build and run package tests")

let with_doc = Arg.(value & flag & info [ "with-doc" ] ~docv:"WITH-DOC" ~doc:"Build package documentation")

let with_dev_setup = Arg.(value & flag & info [ "with-dev-setup" ] ~docv:"WITH-DEV-SETUP" ~doc:"Run package dev setup")

let cmd =
  let doc = "Installs an opam package in the default switch. No checks, no questions asked." in
  let info = Cmd.info "opam-build" ~version:"%%VERSION%%" ~doc in
  Cmd.v info Term.(const build $ verbose_level $ debug_level $ package_name $ with_test $ with_doc $ with_dev_setup)

let () = exit (Cmd.eval' cmd)
