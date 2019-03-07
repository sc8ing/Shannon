let explode s =
  let n = String.length s in
  let rec repeat i =
    match i = n with
    | true  -> []
    | false -> s.[i] :: repeat (i + 1)
  in
  repeat 0

let implode chars =
  let res = Bytes.create (List.length chars) in
  let rec repeat i chars =
    match chars with
    | [] -> res
    | char :: chars ->
      Bytes.set res i char;
      repeat (i + 1) chars
  in
  Bytes.to_string (repeat 0 chars)

(* findPat : 'a list -> 'a list -> 'a list
 * return list of elements that come after pattern in input *)
let rec findPat i p =
  if (List.length p) >= (List.length (i))
  then failwith "findPat: pattern longer than input"
  else
    let rec areSame is ps =
      match (is, ps) with
      | (i::is, p::ps) -> if i=p then areSame is ps else false
      | ([], []) -> false
      | (_, []) -> true
      | (_, _) -> false
    in
    let rec loop i p ans =
      match i with
      | [] -> ans
      | i::is ->
        let ans' =
          if not (areSame (i::is) p) then ans
          else let nextChar = List.nth (i::is) (List.length p) in nextChar::ans
        in loop is p ans'
    in
    loop i p []

(* getNth : 'a list -> int -> 'a list
 * return the first n elements of a list *)
let rec getNth cs n =
  let rec loop cs n ans =
    if n=0 then ans else
    match cs with
    | [] -> failwith "getNth: failure"
    | c::cs -> loop cs (n-1) (c::ans)
  in
  List.rev (loop cs n [])

let getLastNth cs n = List.rev (getNth (List.rev cs) n)

(* shannon : input:string -> load:int -> string
 * returns similar text to the input based on the load using a slightly modified version of Claude Shannon's n-gram algorithm for text. rather than taking the first n (n=load) characters it decreases the load to the length of the current output while it's less than the load *)
let rec shannon input load =
  let input = explode input in
  let rec loop ans =
    if (List.length input) < (List.length ans) then ans else
    match ans with
    | [] -> let firstChar = getNth input 1 in loop firstChar
    | c::cs ->
      let load' = if (List.length ans) <= load then List.length ans else load in
      let pattern = getLastNth ans load' in
      let posChars = findPat input pattern in
      match posChars with
      | [] -> ans
      | c::cs ->
        let nextChar = List.nth posChars (Random.int (List.length posChars)) in
        let ans' = ans @ [nextChar] in
        loop ans'
  in
  implode (loop [])
