open Print
open Typing

exception Break


type opts = {
  files: string list
}

let getOpts args =
  { files = args }

let print_delim () = prerr_string (String.make 20 '-' ^ "\n")

let main _ args =
  let opts = getOpts args in
  List.iter
    (fun file ->
      let prog = Parser.program Lexer.main (Lexing.from_channel (open_in file)) in
      pp_print_prog Format.err_formatter prog;
      print_delim ();
      (* print_string "(set-option :produce-models true)\n"; *)
      (* print_string "(set-option :produce-unsat-cores true)\n"; *)
      let vv, cc = Typing.gen_ineq_prog prog in
      List.iter (fun v -> print_string ("(declare-const f_" ^ string_of_int v ^ " Real) (assert (and (<= 0. f_" ^ string_of_int v ^ ") (<= f_" ^ string_of_int v ^ " 1.)))\n")) vv;
      let n = ref 0 in
      List.iter (fun c -> print_string ("(assert (! " ^ string_of_ineq c ^ " :named c_" ^ string_of_int (incr n; !n) ^ "))\n")) cc;
      (* print_string "(check-sat)\n"; *)
      (* print_string "(get-model)\n"; *)
    )
    opts.files

let _ =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Break));
  main Sys.argv.(0) (List.tl (Array.to_list Sys.argv))
