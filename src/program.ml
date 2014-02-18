type var = int

let fresh_var =
  let n = ref 0 in
  fun () ->
    let i = !n in
    incr n; i
