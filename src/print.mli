val pp_paren : bool -> Format.formatter -> (Format.formatter -> unit) -> unit
val pp_list :
  string ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
val pp_map : ('a -> 'b) -> (Format.formatter -> 'b -> unit) -> Format.formatter -> 'a -> unit

val pp_var_with_id : Format.formatter -> Syntax.var * Syntax.varid -> unit
val pp_var : Format.formatter -> Syntax.var -> unit
val pp_print_comm : Format.formatter -> Syntax.comm -> unit
val print_comm : Syntax.comm -> unit
val pp_print_prog : Format.formatter -> Syntax.prog -> unit
val print_prog : Syntax.prog -> unit
