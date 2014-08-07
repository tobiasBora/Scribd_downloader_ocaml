(** This module is used to provide easy to use functions to deal with files.
    This module depends on the module sequence. Thank you to
    Simon Cruanes for his help.
 *)

module S = Sequence

let seq_of_ic ic = fun f -> try while true do f (input_line ic) done
			      with End_of_file -> ()
(** Create a sequence from an input channel *)
						    
let seq_of_file filename =
  fun f ->
  let ic = open_in filename in
  try
    while true do
      f (input_line ic)
    done
  with e -> (close_in ic; if e <> End_of_file then raise e)
(** Create a sequence from a filename. Close the file when the sequence is consumed. *)

let write_in_oc oc seq = S.iter (fun s -> output_string oc (s ^ "\n")) seq
(** Write the sequence in an output channel *)

let write_in_file ?(mode=[Open_wronly; Open_creat; Open_trunc; Open_text]) ?(perm=0o664) filename seq =
  let oc = open_out_gen mode perm filename in
  S.iter (fun s -> output_string oc (s ^ "\n")) seq;
  flush oc;
  close_out oc
(** Write the sequence in a file (auto close it). By default open it with right 0o664 and mode [Open_wronly; Open_creat; Open_trunc; Open_text].*)
	    
