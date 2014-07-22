(** This library should download documents from the website
     Scridb and convert them in PDF *)
open Batteries
open Lwt       
let (//) = Filename.concat

(** ===== Configuration ===== *)
type conf =
    {
      phantomjs:string;
      convert:string;
      zoom:int;
      tmp_folder:string;
      out_file:string;
      size_group_page:int;
      debug_mode:bool;
      verbose:int;
    }

(** ===== Initialisation ===== *)
let get_conf
      ?(phantomjs="phantomjs")
      ?(convert="convert")
      ?(zoom=2)
      ?(tmp_folder=".tmp/")
      ?(out_file="")
      ?(size_group_page=10)
      ?(debug_mode=true)
      ?(verbose=3)
      ()
  =
  { phantomjs; convert; zoom; tmp_folder; out_file; size_group_page; debug_mode; verbose}

let init_folder pr conf =
  pr 5 ("Removing folder " ^ conf.tmp_folder);
  FileUtil.(rm ~recurse:true [conf.tmp_folder]);
  pr 5 ("Creating folder " ^ conf.tmp_folder);
  FileUtil.mkdir conf.tmp_folder;
  pr 5 ("Going into " ^ conf.tmp_folder);
  Sys.chdir conf.tmp_folder;
  pr 4 ("Current working folder:" ^ Sys.getcwd ())

(** ===== Some useful functions ===== *)

let map_file f file_in file_out =
  BatFile.lines_of file_in
  |> BatEnum.map f
  |> BatFile.write_lines (file_out ^ ".tmp");
  (try Sys.remove file_out with _ -> ());
  Sys.rename (file_out ^ ".tmp") (file_out)

let run_command ?(prog="") pr args outfile =
  (* let open Scribd_downloader in *)
  Array.fold_left ( ^ ) "" args |> pr 4;
  Lwt_process.pread (prog, args)
  >|= (fun str -> BatFile.write_lines outfile (BatEnum.singleton str))

let match_reg reg str =
  try let _ = Str.search_forward reg str 0 in true with Not_found -> false

(* The following functions are usefull to remove some html nodes *)
let filter_nodes f reg_str_begin reg_str_classic_begin reg_str_end filename_in filename_out =
  let reg_b = Str.regexp reg_str_begin in
  let reg_m = Str.regexp reg_str_classic_begin in
  let reg_e = Str.regexp reg_str_end in
  BatFile.lines_of filename_in
  |> BatEnum.filter (f reg_b reg_m reg_e)
  |> BatFile.write_lines (filename_out ^ ".tmp");
  (try Sys.remove filename_out with _ -> ());
  Sys.rename (filename_out ^ ".tmp") filename_out

let remove_nodes =
  let f reg_b reg_m reg_e =
    let i = ref 0 in
    let aux line =
      if match_reg reg_b line then (incr i; false)
      else if !i > 0 && match_reg reg_m line  then (incr i; false)
      else if !i > 0 && match_reg reg_e line then (decr i; false)
      else !i = 0
    in aux
  in
  filter_nodes f

let keep_n_nodes n =
  let f reg_b reg_m reg_e =
    let i = ref 0 in (* Count parenthesis *)
    let gr = ref n in (* Count group *)
    let aux line =
      let action =
	if !i = 0 && match_reg reg_b line  then (incr i; true)
	else if !i > 0 && match_reg reg_m line then (incr i; true)
	else if !i > 0 && match_reg reg_e line then (decr i; true)
	else false
      in
      if !i = 0 && action then decr gr;
      (!gr > 0) || (!i = 0)
    in aux
  in
  filter_nodes f

let remove_n_nodes n =
  let f reg_b reg_m reg_e =
    let i = ref 0 in (* Count parenthesis *)
    let gr = ref n in (* Count group *)
    let aux line =
      let action =
	if !gr > 0 then
	  if !i = 0 && match_reg reg_b line then (incr i; true)
	  else if !i > 0 && match_reg reg_m line then (incr i; true)
	  else if !i > 0  && match_reg reg_e line then (decr i; true)
	  else false
	else false
      in
      if !i = 0 && action then decr gr;
      (not action) && ((!gr <= 0) || (!i = 0))
    in aux
  in
  filter_nodes f

(* ========================= *)
(* ======= Main Part ======= *)
(* ========================= *)
(* The following functions supposed you are in the temp folder *)

(** Download a first version to get useful information *)
let step_1_download_page pr conf url =
  BatFile.write_lines
    "phantom_step1.js"
    (BatEnum.singleton ("var page = require('webpage').create();
url = \"" ^ url ^ "\"
page.open(url, function () {
console.log(page.content);
phantom.exit();
});
// Avoid error messages
page.onError = function(msg, trace) {
};"));
  run_command
    pr
    [|
      conf.phantomjs;
      "--load-images=no";
      "phantom_step1.js";
    |]
    "page.html"
	   
(** We get the title *)
let step_2_get_title conf =
  let reg = Str.regexp "<title>\\(.*\\)</title>" in
  let get_title line =
    try
      let _ = Str.search_forward reg line 0 in
      Some (Str.matched_group 1 line)
    with _ -> None
  in
  BatFile.lines_of "page.html"
  |> BatEnum.find_map get_title

(** A first way to count pages *)
(* let step_3_get_nb_pages conf = *)
(*   let reg = Str.regexp ".*document.getElementById(\"outer_page.*" in *)
(*   let is_page line = match_reg reg line in *)
(*   BatFile.lines_of "page.html" *)
(*   |> BatEnum.filter is_page *)
(*   |> BatEnum.count *)
(** Each page can have a different size.
We get here the size of every single page *)
let step_3_get_pages_size conf =
  let reg = Str.regexp "var pageParams = {\"origHeight\": \\([0-9]+\\), \"origWidth\": \\([0-9]+\\)" in
  let get_width line =
    try
      let _ = Str.search_forward reg line 0 in
      Some (Str.(matched_group 1 line |> int_of_string
		, matched_group 2 line |> int_of_string))
    with _ -> None
  in
  let pages_size_enum = BatFile.lines_of "page.html"
	      |> BatEnum.filter_map get_width in
  let n_tot = BatEnum.count pages_size_enum in
  let max_h = pages_size_enum |> BatEnum.clone
	      |> BatEnum.arg_max (fun (h,w) -> h) |> fst in
  let max_w = pages_size_enum |> BatEnum.clone
	      |> BatEnum.arg_max (fun (h,w) -> w) |> snd in
  (n_tot, pages_size_enum, (max_h, max_w))


(** Usefull in debug mode to check that pages are well aligned *)
let color_pages nb_pages =
  let oc = open_out ~mode:[`append] "page.html" in
  Printf.fprintf oc "<style>\n";
  let r () = Random.int 256 in
  for i = 1 to nb_pages do
    Printf.fprintf oc
		   "#outer_page_%d {background-color: rgb(%d,%d,%d);}"
		   i (r ()) (r ()) (r ());
  done;
  Printf.fprintf oc "</style>";
  flush oc

(* We clear the html page to keep only usefull parts *)
let step_4_removing_useless_parts conf =
  (* We make a new line for each html element and remove margins *)
  let reg_begin = Str.regexp "<" in
  let reg_end = Str.regexp ">" in
  let reg_cont = Str.regexp "id=\"doc_container\"" in
  let reg_out = Str.regexp "<div class=\"outer_page" in
  let blur_page = Str.regexp "blurred_page" in
  let scribd_load = Str.regexp ".*Scribd.ReadPage.early_initialize.*" in
  
  map_file (fun s ->
	    Str.global_replace reg_begin "\n<" s
	    |> Str.global_replace reg_end ">\n")
	   "page.html"
	   "page.html";
  map_file (fun s -> s
	    |> Str.global_replace reg_cont "id=\"doc_container\" style=\"min-width:0px;margin : 0px; width:100%;\""
	    |> Str.global_replace reg_out "<div style=\"margin: 0px;\" class=\"outer_page"
	    |> Str.global_replace blur_page ""
	    |>  Str.(global_replace scribd_load "")
       )
      "page.html"
      "page.html";

  let reg_list =
    [
      "<div.*id=\"global_header";
      "<div.*class=\"header_spacer";
      "<div.*class=\"toolbar_spacer";
      "<div.*id=\"grab_blur_promo_here";
      "<div.*id=\"doc_sidebar";
      "<div.*id=\"leaderboard_ad_main";
      "<div.*id=\"doc_info";
      "<div.*id=\"between_page_ads";
      "<div.*id=\"page_missing_explanation";
      "<div.*class=\"b_..";
      "missing_page_buy_button";
      "<div class=\"buy_doc_bar";
      "<div class=\"shadow_overlay\">";
    ]
  in
  reg_list
  |> List.iter (fun r -> remove_nodes r "<div" "</div" "page.html" "page.html")


(** The JS must be change to have the good size : *)
let step_5_ajust_size conf (max_h, max_w) =
  map_file (Str.global_replace
	      (Str.regexp "var defaultViewWidth = defaultViewWidth.*")
	      ("var defaultViewWidth = defaultViewWidth || " ^
		 (string_of_int max_w) ^
		   ";"))
	   "page.html"
	   "page.html"
  			
(** The script download pages 10 by 10 by default to avoid memory errors.
This function remove/keep the goods ones *)
let step_6_aux_extract_group conf =
  let rem_g n = remove_n_nodes n "id=\"outer_page_" "<div" "</div" in
  let keep_g n = keep_n_nodes n "id=\"outer_page_" "<div" "</div" in

  keep_g conf.size_group_page "page.html" "group_only.html";
  rem_g conf.size_group_page "page.html" "page.html"

(** Convert the current group of pages into png *)
let step_6_aux_get_whole_png pr conf nb_pages_to_convert (max_h,max_w) =
  let n = nb_pages_to_convert in
  BatFile.write_lines
    "phantom_step6.js"
    (BatEnum.singleton ("var page = require('webpage').create();
output='out.png';
address = 'group_only.html';
nb_pages = " ^ string_of_int n ^ ";
zoom = " ^ string_of_int conf.zoom ^ ";
width = " ^ string_of_int max_w ^ " * zoom;
height = (768+" ^ (Printf.sprintf "%d*%d" max_h n) ^ ")* zoom;
page.viewportSize = { width: width, height: height };
page.zoomFactor = zoom;
page.open(address, function (status) {
if (status !== 'success') {
console.log('Unable to load the address!');
} else {
page.clipRect = { top: 0, left: 0, width: width, height: height };
window.setTimeout(function () {
page.render(output);
phantom.exit();
}, 200);
}
});
// Avoid error messages
page.onError = function(msg, trace) {
};"));
  run_command
    pr
    [|
      conf.phantomjs;
      "phantom_step6.js";
    |]
    "poubelle"


(** Convert the current group into png *)
(** TODO : Try to avoid the "convert" dependence *)
let step_6_aux_get_individual_png pr conf nb_page_to_convert page_size_enum next_name0 =
  let nb_pages = nb_page_to_convert in
  let current_h = ref 0 in
  let rec loop i next_name =
    if i = 0 then Lwt.return next_name
    else 
      let (h,w) = BatEnum.get_exn page_size_enum in
      let h_z = h * conf.zoom
      and w_z = w * conf.zoom in
      pr 5 (Printf.sprintf "Name : %d, Height: %d, Width %d" next_name w h);
      run_command
	pr
	[|
	  conf.convert;
	  "out.png";
	  "-gravity";
	  "NorthWest";
	  "-crop";
	  Printf.sprintf "%dx%d+0+%d" w_z h_z !current_h;
	  "-quality";
	  "100";
	  "-compress";
	  "jpeg";
	  "-gravity";
	  "center";
	  "-resize";
	  Printf.sprintf "%dx%d" (1240 * conf.zoom) (1753 * conf.zoom);
	  "-extent";
	  Printf.sprintf "%dx%d" (1240 * conf.zoom) (1753 * conf.zoom);
	  "-gravity";
	  "SouthWest";
	  "-page";
	  "a4";
	  Printf.sprintf "0_%05d.pdf" next_name;
	 |]
	"phantom_step.js"
      >|= (fun _ -> current_h := !current_h + h_z)
      >>= (fun _ -> loop (i-1) (next_name + 1))
  in
  loop nb_pages next_name0

(** This function convert every page into pdf (name : 0_<num page>.pdf) *)
let step_6_get_group_page pr pourcent conf pages_size_enum max_sizes =
  let n_tot = BatEnum.count pages_size_enum in
  let rec loop next_name =
    if BatEnum.is_empty pages_size_enum then
      Lwt.return_unit
    else
      begin
	let nr = BatEnum.count pages_size_enum in
	let nd = min conf.size_group_page nr in
	pr 3 (Printf.sprintf "Downloading %d pages (%d/%d already downloaded, %d left) ==> %d %%" nd (n_tot - nr) n_tot nr (((n_tot-nr)*100)/n_tot));
	pourcent n_tot nr;
	step_6_aux_extract_group conf;
	pr 3 " Extracting";
	step_6_aux_get_whole_png pr conf nd max_sizes
	>>= fun _ ->
	step_6_aux_get_individual_png pr conf nd pages_size_enum next_name
	>>= loop
      end
  in
  loop 0

(* This function put together all pdf and put the result in final.pdf *)
let step_7_generate_pdf pr conf =
  let file_list =
    FileUtil.(ls "." |> filter Is_file)
    |> BatSet.of_list (*Alphabetic order *)
    |> BatSet.enum
    |> BatEnum.filter (fun str -> match_reg (Str.regexp "0_.*\\.pdf$") str)
    |> BatList.of_enum
  in
  let pdf_list =
    file_list
    |> List.map (fun file -> Pdfread.pdf_of_file None None file)
  in
  let range =
    file_list
    |> List.map (fun pdf -> [1]) (* Documents only have one page*)
  in
  Pdfmerge.merge_pdfs true true file_list pdf_list range
  |> (fun pdf -> Pdfwrite.pdf_to_file pdf "final.pdf")


