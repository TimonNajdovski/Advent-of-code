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

  let lines = String.split_on_char '\n'
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
      | _ -> failwith ":p"
  

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


let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
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