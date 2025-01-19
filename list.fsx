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

let append lst1 lst2 =
  lst1 @ lst2

let average lst =
  match lst with
  | [] -> failwith "empty list"
  | _ -> List.sum lst / List.length lst 

let averageBy projection lst =
  match lst with
  | [] -> failwith "empty list"
  | x ->
    List.sum (List.map projection lst) / List.length lst
    
[<TailCall>]
let choose chooser lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      match chooser h with
      |Some x -> inner t (x::acc)
      |None -> inner t acc
  inner lst []

[<TailCall>]
let chunkBySize chunkSize lst =
  let rec inner lst acc chunk =
    match lst with
    | [] -> acc |> List.rev
    | h::t ->
      if List.length chunk = chunkSize then
        inner t (chunk::acc) [h]
      else inner t acc (h::chunk)
  inner lst [] []

[<TailCall>]
let collect mapping lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc |> List.rev
    | h::t ->
      inner t ((mapping h)::acc)
  inner lst []