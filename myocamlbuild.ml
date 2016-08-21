open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "pipes"
let version = "dev"

let make_lib ?findlib_deps ?internal_deps ?ml_files ?mli_files lib_name : Project.item =
  let name = sprintf "%s-%s" project_name lib_name in
  Project.lib name
    ~pkg:(sprintf "%s.%s" project_name lib_name)
    ~dir:(sprintf "lib/%s" lib_name)
    ~style:(`Pack (String.map (function '-' -> '_' | c -> c) name))
    ?findlib_deps
    ?internal_deps
    ?ml_files
    ?mli_files

let make_app ?findlib_deps ?internal_deps name : Project.item =
  Project.app name
    ~file:(sprintf "app/%s.ml" name)
    ?findlib_deps
    ?internal_deps

let pure = make_lib "pure"

let parsers = make_lib "parsers"
    ~internal_deps:[pure]
    ~findlib_deps:["core_kernel"]

let benchmark =
  make_lib "benchmark"
    ~internal_deps:[pure]
    ~findlib_deps:["cfstream" ; "core_bench"]

let libs = [pure ; parsers ; benchmark]
let apps = [
  make_app "bench" ~internal_deps:[benchmark]
]

let optional_pkgs = ["async"; "lwt"]

let items =
  List.filter ~f:(fun x -> Project.dep_opts_sat x optional_pkgs)
    libs@apps

let ocamlinit_postfix = [
]

;;
let () =
  let open Solvuu_build.Std.Project in

  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let annot = Some () in
  let bin_annot = Some () in
  let g = Some () in
  let safe_string = Some () in
  let short_paths = Some () in
  let thread = Some () in
  let w = Some "A-4-33-41-42-44-45-48" in

  let items = List.map items ~f:(function
    | Lib x ->
      Lib {x with annot; bin_annot; g; safe_string; short_paths; thread; w}
    | App x ->
      App {x with annot; bin_annot; g; safe_string; short_paths; thread; w}
  )
  in

  let libs = filter_libs items in
  let apps = filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.Before_options -> (
      Ocamlbuild_plugin.Options.use_ocamlfind := true
    )
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      Tools.m4_rule ()
        ~_D:[
          "GIT_COMMIT", Some (match Tools.git_last_commit() with
            | None -> "None"
            | Some x -> sprintf "Some \"%s\"" x
          );
          "VERSION", Some version;
        ];

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      Findlib.build_meta_file (meta_file ~version libs);
      build_static_file ".merlin" (merlin_file items);
      build_static_file (sprintf "%s.install" project_name)
        (install_file items);
      build_static_file ".ocamlinit"
        (ocamlinit_file items ~postfix:ocamlinit_postfix);
      build_static_file "project.mk"
        (makefile items ~project_name);
    )
  | _ -> ()
