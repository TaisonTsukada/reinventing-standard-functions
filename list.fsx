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

[<TailCall>]
let compareWith comparer lst1 lst2 =
  let rec inner lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | h1::t1, h2::t2 ->
      let c = comparer h1 h2
      if c =0 then inner t1 t2
      else c
  inner lst1 lst2
  
[<TailCall>]
let concat lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc |> List.rev
    | h::t -> inner t (h @ acc)
  inner lst []

[<TailCall>]
let rec contains v source =
  match source with
  | [] -> false
  | h::t -> if h = v then  true else contains v t

[<TailCall>]
let countBy projection lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc |> List.rev
    | h::t ->
      let key = projection h
      let count = acc |> List.tryFind (fun (k, _) -> k = key)
      match count with
      | Some (k, c) -> inner t ((k, c + 1)::(acc |> List.filter (fun (k, _) -> k <> key)))
      | None -> inner t ((key, 1)::acc)
  inner lst []

[<TailCall>]
let distinct lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc |> List.rev
    | h::t ->
      if contains h acc then inner t acc
      else inner t (h::acc)
  inner lst []

[<TailCall>]
let distictBy projection lst =
  let rec inner lst acc =
    match lst with
    | [] -> acc |> List.rev
    | h::t ->
      if projection h |> contains acc then inner t acc
      else inner t (h::acc)
  inner lst []

let empty () = []

let exactlyOne lst =
  match lst with
  | [] -> failwith "empty list"
  | [x] -> x
  | _ -> failwith "more than one element"
  
[<TailCall>]
let except itemsToExclude lst =
  let rec inner lst2 acc =
    match lst2 with
    | [] -> acc |> List.rev
    | h::t ->
      if contains h itemsToExclude then inner t acc
      else inner t (h::acc)
  inner lst []
  
let rec exists predicate lst =
  match lst with
  | [] -> false
  | h::t -> if predicate h then true else exists predicate t
  
let rec exists2 predicate lst1 lst2 =
  match lst1, lst2 with
  | [], [] -> false
  | _, [] -> failwith "lists are not of the same length"
  | [], _ -> failwith "lists are not of the same length"
  | h1::t1, h2::t2 -> if predicate h1 h2 then true else exists2 predicate t1 t2 