[<TailCall>]
let allPairs lst1 lst2 =
  let rec inner lst1 lst2 acc =
     match lst1 with
     | [] -> acc |> List.rev
     | h1::t1 ->
      let rec inner2 lst2 acc =
        match lst2 with
        | [] -> acc
        | h2::t2 -> inner2 t2 ((h1, h2)::acc)
      inner2 lst2 acc
  inner lst1 lst2 []


let people = [ "Kirk"; "Spock"; "McCoy" ]
let numbers = [ 1; 2 ]