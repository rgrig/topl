let make () = Hashtbl.create 13

let rec rep h x =
  try
    let y = Hashtbl.find h x in
    let z = rep h y in
    Hashtbl.replace h x z; z
  with Not_found -> x

let union h x y =
  let x, y = if Random.bool () then x, y else y, x in
  Hashtbl.replace h (rep h x) y

let equals h x y = rep h x = rep h y
