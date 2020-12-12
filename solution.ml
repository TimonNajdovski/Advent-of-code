#load "str.cma"
let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let split_on_blank string =
    let lines = String.split_on_char '\n' string in
    let rec aux acc_l acc_s = function
      | [] -> acc_s :: acc_l
      | x :: xs -> 
        if x = "" then
          aux (acc_s :: acc_l) "" xs
        else if acc_s = "" then
          aux acc_l x xs
        else  
          aux acc_l (acc_s ^ " " ^ x) xs
    in
    aux [] "" lines

  let list_of_string string =
    List.init (String.length string) (String.get string)
  
  let lines = String.split_on_char '\n' 

  let intersection lists =
    
    let rec aux element = function
      | [] -> true
      | x :: xs -> 
        if List.mem element x then
          aux element xs
        else
          false
    in      
    let rec intersection_aux acc lists = function
      | [] -> acc
      | y :: ys ->
        if y = ' ' then
          intersection_aux acc lists ys
        else if aux y lists then
          intersection_aux (y :: acc) lists ys
        else
          intersection_aux acc lists ys
    in
    match lists with
      | [] -> failwith "no lists"
      | x :: xs -> 
        intersection_aux [] xs x
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end

module Solver1 : Solver = struct
  
  let rec pair_sums_to n = function
    | x :: xs -> 
      let list = List.map (fun y -> x + y) xs in
      if List.mem n list then
        (x, n - x)
      else
        pair_sums_to n xs
    
    | [] -> (0, 0)

  let naloga1 data =
    let list = data |> List.lines |> List.int_list in
    let (x, y) = pair_sums_to 2020 list in
    string_of_int (x * y)

  let naloga2 data _part1 =
    let list = data |> List.lines |> List.int_list in
    let rec aux = function
      | [] -> failwith "Prazno"
      | x :: xs -> 
        match pair_sums_to (2020 - x) xs with
        | (0, 0) -> aux xs
        | (y, z) -> x*y*z
    in
    string_of_int(aux list
    )          
end

module Solver2 : Solver = struct

  let extract string = 
      match String.split_on_char ' ' string with
      | [bounds ; n ; rest] -> 
        match String.split_on_char '-' bounds with
        | [lower ; upper] -> (int_of_string lower, int_of_string upper, String.get n 0, rest)
        | _ -> failwith ":pp"
  

  let extract_on_list list = 
    List.map extract list
  
  let naloga1 data =
    let list = data |> List.lines |> extract_on_list in
    let rec aux_aux counter n = function
      | [] -> counter
      | y :: ys ->
        if y = n then
          aux_aux (counter + 1) n ys
        else
          aux_aux counter n ys
    in
    let rec aux counter = function
      | [] -> counter
      | x :: xs -> 
        match x with
        | (lower, upper, n, chars) ->
          let list_of_chars = List.init (String.length chars) (String.get chars) in
          if (aux_aux 0 n list_of_chars >= lower && aux_aux 0 n list_of_chars <= upper) then
            aux (counter + 1) xs
          else
            aux counter xs
    in
    string_of_int (aux 0 list)
    
    let naloga2 data _part1 =
      let list = data |> List.lines |> extract_on_list in
      let rec aux counter = function
        | [] -> counter
        | x :: xs -> 
          match x with
          | (lower, upper, n, chars) -> 
            if ((String.get chars (lower - 1) = n) <> (String.get chars (upper - 1) = n)) then
              aux (counter + 1) xs
            else
              aux counter xs
      in
      string_of_int (aux 0 list)
end

module Solver3 : Solver = struct

  let get_index n step length =
    (n + step) mod length

  let naloga1 data =
    let rows = data |> List.lines in
    let rec aux counter position length = function
      | [] -> counter
      | x :: xs -> 
        let index = get_index position 3 length in
        if String.get x position = '#' then 
          aux (counter + 1) index length xs
        else
          aux counter index length xs
    in
    string_of_int (aux 0 0 (rows |> List.hd |> String.length) rows)

  let naloga2 data _part1 =
    let rows = data |> List.lines in
    let rec aux counter position step length = function
      | [] -> counter
      | x :: xs -> 
        let index = get_index position step length in
        if String.get x position = '#' then 
          aux (counter + 1) index step length xs
        else
          aux counter index step length xs
    in
    let rec paths product rows = function
      | [] -> product
      | y :: ys -> 
        let new_product = product * (aux 0 0 y (rows |> List.hd |> String.length) rows) in
        paths new_product rows ys
    in
    let rec aux_double skip counter position length = function
      | [] -> counter
      | x :: xs -> 
        if skip then
          aux_double (not skip) counter position length xs
        else
          let index = get_index position 1 length in
          if String.get x position = '#' then 
            aux_double (not skip) (counter + 1) index length xs
          else
            aux_double (not skip) counter index length xs
    in
    string_of_int ((paths 1 rows [1;3;5;7]) * (aux_double false 0 0 (rows |> List.hd |> String.length) rows))
end

module Solver4 : Solver = struct

  let naloga1 data = 
    let list = 
      data |> List.split_on_blank
      |> List.map (fun y -> String.split_on_char ' ' y)
      |> List.map (fun y -> List.map (fun x -> String.sub x 0 3) y)
    in
    let rec checker is_valid list = function
      | [] -> is_valid
      | y :: ys ->
        checker (is_valid && (List.mem y list)) list ys
    in
    let rec aux counter = function
      | [] -> counter
      | x :: xs -> 
        if checker true x  ["byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"]  then
          aux (counter + 1) xs
        else
          aux counter xs
    in
    string_of_int (aux 0 list)

  let naloga2 data _part1 = "0"

end

module Solver5 : Solver = struct

  let binary_to_dec string =
    let list = List.init (String.length string) (String.get string) |> List.rev in
    let rec aux digit sum = function
      | [] -> sum
      | x :: xs ->
        if x = '0' then
          aux (digit * 2) sum xs
        else
          aux (digit * 2) (sum + digit) xs
    in
      aux 1 0 list 

  let process string = 
    let process_row = function
      | 'F' -> '0'
      | 'B' -> '1'
      | _ -> failwith "Invalid row"
    in
    let process_column = function
      | 'L' -> '0'
      | 'R' -> '1'
      | _ -> failwith "Invalid column"
    in
    let row = 
      String.sub string 0 7
      |> String.map (fun y -> process_row y)
    in
    let column = 
      String.sub string 7 3
      |> String.map (fun y -> process_column y)
    in
    (binary_to_dec row, binary_to_dec column)
  
  let naloga1 data =
    let lines = data |> List.lines |> List.map process in
    let rec aux biggest = function
      | [] -> biggest
      | x :: xs ->
        match x with
        | (row, column) ->
          if (row * 8) + column > biggest then
            aux ((row * 8) + column) xs
          else
            aux biggest xs
    in 
    string_of_int (aux 0 lines)

  let naloga2 data _part1 = 
  let lines = 
  data |> List.lines 
  |> List.map process
  |> List.map (fun (x, y) -> (8*x) + y)
  |> List.sort (compare) in
  
  let rec aux index = function
    | [] -> failwith "empty list"
    | [x] -> x
    | x :: xs ->
      if x != index then
        index
      else
        aux (index + 1) xs
  in
  string_of_int (aux (List.hd lines) lines)

end

module Solver6 : Solver = struct

  let naloga1 data =
    let groups =
    data |> List.split_on_blank
    |> List.map (fun y -> List.list_of_string y)

    in
    let rec elements_in_group list = function
      | [] -> List.length list
      | x :: xs ->
        if (x = ' ' || List.mem x list) then
          elements_in_group list xs
        else
          elements_in_group (x :: list) xs
    in
    let rec aux counter = function
      | [] -> counter
      | x :: xs -> 
        aux (counter + (elements_in_group [] x)) xs
    in
    string_of_int(aux 0 groups)

  let naloga2 data _part1 = 
    let groups =
    data |> List.split_on_blank
    |> List.map (fun y -> String.split_on_char ' ' y)
    |> List.map (fun y -> List.map (fun x -> List.list_of_string x) y) in
    let rec aux counter = function
      | [] -> counter
      | x :: xs ->
        aux (counter + List.length (List.intersection x)) xs
    in
    string_of_int(aux 0 groups)
    
end

module Solver12 : Solver = struct

  let process string = 
      (String.get string 0, int_of_string (String.sub string 1 ((String.length string) - 1)))
  
  let move (x, y) distance = function
    | 'N' -> (x, y + distance)
    | 'S' -> (x, y - distance)
    | 'E' -> (x + distance, y)
    | 'W' -> (x - distance, y)
    | _ -> failwith "invalid direction"
  
  let change_facing num facing = 
    let facing_to_num = function
      | 'N' -> 0
      | 'E' -> 1
      | 'S' -> 2
      | 'W' -> 3
      | _ -> failwith "invalid string"
    in
    match ((facing_to_num facing) + num) mod 4 with
      | 0 -> 'N'
      | 1 -> 'E'
      | 2 -> 'S'
      | 3 -> 'W'
      | _ -> failwith "invalid int"

  let rotate facing = function
    | ('R', 90) | ('L', 270) -> change_facing 1 facing
    | ('L', 90) | ('R', 270) -> change_facing 3 facing
    | ('L', 180) | ('R', 180) -> change_facing 2 facing
    | _ -> failwith "invalid input"

  let execute_order ((x, y), facing) = function
    | ('L', rotation) -> ((x, y), rotate facing ('L', rotation))
    | ('R', rotation) -> ((x, y), rotate facing ('R', rotation))
    | ('F', distance) -> ((move (x, y) distance facing), facing)
    | (direction, distance) -> ((move (x, y) distance direction), facing)

  let naloga1 data =
    let list = data |> List.lines |> List.map (fun y -> process y) in
    let rec aux ((x, y), facing) = function
      | [] -> abs x + abs y
      | z :: zs -> aux (execute_order ((x, y), facing) z) zs
    in
    string_of_int (aux ((0, 0), 'E') list)

  let naloga2 data _part1 = "0"

end

let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "12" -> (module Solver12)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()