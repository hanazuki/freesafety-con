type 'a t

val empty: 'a t
val identify: Syntax.var -> 'a t -> Syntax.varid
val lookup: Syntax.var -> 'a t -> 'a
val lookup_by_id: Syntax.varid -> 'a t -> 'a
val bound: Syntax.var -> 'a t -> bool
val extend: (Syntax.var * Syntax.varid) -> 'a -> 'a t -> 'a t
val remove: Syntax.var -> 'a t -> 'a t
val remove_by_id: Syntax.varid -> 'a t -> 'a t
val remove_if: (Syntax.var -> Syntax.varid -> bool) -> 'a t -> 'a t
val update: Syntax.var -> 'a -> 'a t -> 'a t
val update_by_id: Syntax.varid -> 'a -> 'a t -> 'a t
val remove_all: Syntax.var -> 'a t -> 'a t
val names: 'a t -> Syntax.var list
val ids: 'a t -> Syntax.varid list
val unname: 'a t -> (Syntax.varid * 'a) list
val fold: (Syntax.var * Syntax.varid -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
val iter: (Syntax.var -> Syntax.varid -> 'a -> unit) -> 'a t -> unit
