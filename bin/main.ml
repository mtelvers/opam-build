open Cmdliner

let init_opam verbose_level debug_level =
  OpamSystem.init ();
  let yes = Some (Some true) in
  let confirm_level = Some `unsafe_yes in
  let verbose_level = if List.is_empty verbose_level then None else Some (List.length verbose_level) in
  let debug_level = if List.is_empty debug_level then None else Some (List.length debug_level) in
  OpamCoreConfig.init ?yes ?confirm_level ?verbose_level ?debug_level ();
  OpamFormatConfig.init ();
  OpamClientConfig.init ()

let with_switch_state f =
  let gt = OpamGlobalState.load `Lock_none in
  let switches = OpamGlobalState.switches gt in
  let switch = List.hd switches in
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_write gt ~switch f

let rec find_opam_files dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.concat_map (fun name ->
         let path = Filename.concat dir name in
         if Sys.is_directory path then
           if name = "_build" || name = "_opam" || (String.length name > 0 && name.[0] = '.') then []
           else find_opam_files path
         else if Filename.check_suffix name ".opam" then
           [ (Filename.remove_extension name, path) ]
         else [])
  with Sys_error _ -> []

let load_local_opams version =
  find_opam_files (Sys.getcwd ())
  |> List.map (fun (pkg_name, path) ->
       let pkg = OpamPackage.create (OpamPackage.Name.of_string pkg_name) (OpamPackage.Version.of_string version) in
       let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path))
         |> OpamFile.OPAM.with_name (OpamPackage.name pkg)
         |> OpamFile.OPAM.with_version (OpamPackage.version pkg) in
       (pkg, opam))

let build_local verbose_level debug_level name test doc dev_setup build_only =
  init_opam verbose_level debug_level;
  with_switch_state (fun st ->
      let pkg = OpamPackage.of_string name in
      let cwd = Sys.getcwd () in
      let local_opams = load_local_opams (OpamPackage.version_to_string pkg) in
      let st = { st with opams = List.fold_left (fun acc (p, o) -> OpamPackage.Map.add p o acc) st.opams local_opams } in
      (* Find the directory containing this package's .opam file *)
      let pkg_src_dir =
        match List.find_opt (fun (n, _) -> n = OpamPackage.name_to_string pkg) (find_opam_files cwd) with
        | Some (_, path) -> Filename.dirname path
        | None -> cwd
      in
      let build_dir =
        if build_only then OpamFilename.Dir.of_string pkg_src_dir
        else
          let d = OpamFilename.SubPath.(OpamPath.Switch.build_dir st.switch_global.root st.switch / of_string name) in
          OpamFilename.copy_dir ~src:(OpamFilename.Dir.of_string pkg_src_dir) ~dst:d;
          d
      in
      OpamAction.build_package st ~test ~doc ~dev_setup build_dir pkg |> OpamProcess.Job.run |> function
      | Some exn ->
          Printf.printf "build failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
          1
      | None ->
          if build_only then 0
          else (
            OpamAction.install_package st ~test ~doc ~dev_setup ~build_dir pkg |> OpamProcess.Job.run |> function
            | Right exn ->
                Printf.printf "install failed... %s\n" (OpamStd.Exn.pretty_backtrace exn);
                1
            | Left conf ->
                let conf_files =
                  let add_conf conf = OpamPackage.Name.Map.add pkg.name conf st.conf_files in
                  OpamStd.Option.map_default add_conf st.conf_files conf in
                ignore (OpamSwitchAction.add_to_installed { st with conf_files } pkg);
                0))

let build verbose_level debug_level name test doc dev_setup =
  init_opam verbose_level debug_level;
  with_switch_state (fun st ->
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
      match OpamSwitchState.depexts_unavailable st pkg with
      | Some missing ->
          OpamSolution.print_depext_msg { OpamSysPkg.status_empty with s_not_found = missing };
          1
      | None ->
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

let package_name = Arg.(required & pos 0 (some string) None & info ~docv:"PACKAGE-NAME" ~doc:"The name and version of the package in the format pkg.x.y.z." [])

let verbose_level = Arg.(value & flag_all & info [ "v"; "verbose" ] ~docv:"VERBOSELEVEL" ~doc:"Increase the log level. Use several times to increase, e.g. '-vv'")

let debug_level = Arg.(value & flag_all & info [ "d"; "debug" ] ~docv:"DEBUGLEVEL" ~doc:"Increase the debug level. Use several times to increase, e.g. '-dd'")

let with_test = Arg.(value & flag & info [ "t"; "with-test" ] ~docv:"WITH-TEST" ~doc:"Build and run package tests")

let with_doc = Arg.(value & flag & info [ "with-doc" ] ~docv:"WITH-DOC" ~doc:"Build package documentation")

let with_dev_setup = Arg.(value & flag & info [ "with-dev-setup" ] ~docv:"WITH-DEV-SETUP" ~doc:"Run package dev setup")

let build_only_flag = Arg.(value & flag & info [ "build-only" ] ~doc:"Only run the build commands, skip install")

let local_cmd =
  let doc = "Build a local package from its .opam file in the current directory" in
  let info = Cmd.info "local" ~doc in
  Cmd.v info Term.(const build_local $ verbose_level $ debug_level $ package_name $ with_test $ with_doc $ with_dev_setup $ build_only_flag)

let () =
  let doc = "Installs an opam package in the default switch. No checks, no questions asked." in
  let info = Cmd.info "opam-build" ~version:"%%VERSION%%" ~doc in
  let default_term = Term.(const build $ verbose_level $ debug_level $ package_name $ with_test $ with_doc $ with_dev_setup) in
  let cmd = Cmd.group ~default:default_term info [ local_cmd ] in
  exit (Cmd.eval' cmd)
