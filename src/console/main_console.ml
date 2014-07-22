(** This program is a command line interface for the Scribd_download lib *)

let print_v conf level str =
  let open Scribd_downloader in
  if level <= conf.verbose then Printf.printf "%s\n%!" str


let download_script phantomjs convert zoom tmp_folder out_file size_group_page debug_mode verbose url =
  (* We degine global configuration *)
  let open Scribd_downloader in
  let open Lwt in
  Lwt_main.run begin
      let pwd = Sys.getcwd () in
      let conf = get_conf ~phantomjs ~convert ~zoom ~tmp_folder ~out_file ~size_group_page ~debug_mode ~verbose () in
      let pr = print_v conf in
      (pr 2 "-- Initializing folder...";
      init_folder pr conf;
      pr 2 "-- Downloading a first version to get useful information...";
      pr 2 "(Don't worry if it's a bit long)";
      step_1_download_page pr conf url)
      >>= fun () ->
      let title = step_2_get_title conf in
      pr 2 ("Title: " ^ title);
      let (n_tot, pages_size_enum, max_sizes) =	step_3_get_pages_size conf in
      pr 2 (Printf.sprintf "Nb of pages: %d" n_tot);
      pr 2 (Printf.sprintf "Max height: %d, Max width: %d" (fst max_sizes) (snd max_sizes));
      if conf.debug_mode then
	(pr 2 "-- Adding background color [debug mode]";
	 color_pages n_tot);
      pr 2 "-- Removing useless parts";
      step_4_removing_useless_parts conf;
      pr 2 "-- Resizing";
      step_5_ajust_size conf max_sizes;
      pr 2 "-- Extracting each pages one by one (can take several minutes)";
      step_6_get_group_page pr (fun _ _ -> ()) conf pages_size_enum max_sizes
      >>= fun () ->
      pr 2 "-- Putting pages together...";
      step_7_generate_pdf pr conf;
      pr 2 "-- Copying the pdf in the good folder";
      Sys.chdir pwd;
      let out_pdf =
	conf.out_file
	|> Str.global_replace (Str.regexp "##TITLE##") title
	|> Str.global_replace (Str.regexp " ") "_"
	|> Str.global_replace (Str.regexp "[^a-zA-Z0-9_.-]()") "" in
      Sys.rename (conf.tmp_folder // "final.pdf") out_pdf;
      pr 1 (Printf.sprintf "The file was successfully downloaded in %s." out_pdf);
      if not conf.debug_mode then
	FileUtil.(rm ~recurse:true [conf.tmp_folder]);
      Lwt.return_unit
    end

(* Command line part *)
open Cmdliner
       
let phantomjs = 
  let doc = "Command to run phantomjs." in
  Arg.(value & opt string "phantomjs" & info ["phantomjs"] ~docv:"EXEC" ~doc)

let convert = 
  let doc = "Command to run convert (from ImageMagick)." in
  Arg.(value & opt string "convert" & info ["convert"] ~docv:"EXEC" ~doc)

let zoom = 
  let doc = "Zoom in the document. The more high it is, the more precise the document will be." in
  Arg.(value & opt int 2 & info ["z"; "zoom"] ~docv:"ZOOM" ~doc)

let tmp_folder = 
  let doc = "Tmp folder." in
  Arg.(value & opt string ".tmp" & info ["tmp_folder"] ~docv:"FOLDER" ~doc)

let out_file = 
  let doc = "Output pdf file. Replace ##TITLE## by the name of the document." in
  Arg.(value & opt string "##TITLE##.pdf" & info ["o"; "out_file"] ~docv:"FILE" ~doc)

let size_group_page = 
  let doc = "The number of pages downloaded together. It can cause troubles if it's to high (usually < 20)." in
  Arg.(value & opt int 10 & info ["size_group"] ~docv:"N" ~doc)

let debug_mode = 
  let doc = "Switch to debug_mode" in
  Arg.(value & flag & info ["d";"debug_mode"] ~doc)

let verbose = 
  let doc = "Verbosity (between 0 (nothing) and 5 (everything))." in
  Arg.(value & opt int 3 & info ["v"; "verbose"] ~docv:"N" ~doc)

let url = 
  let doc = "The url of the scribd page." in
  let none = "You need to specify an url to download." in
  Arg.(required & pos 0 (some ~none string) None & info [] ~docv:"URL" ~doc)

let download_script_t = Term.(pure download_script $ phantomjs $ convert $ zoom $ tmp_folder $ out_file $ size_group_page $ debug_mode $ verbose $ url)
		   
let info = 
  let doc = "print a customizable message repeatedly" in
  let man = [ `S "BUGS"; `P "Email bug reports to <tobias.bora at gmail.com>.";] in
  Term.info "Scribd_downloader" ~version:"2.0" ~doc ~man

let () =
  match Term.eval (download_script_t, info)
  with `Error _ -> exit 1
     | _ -> exit 0
