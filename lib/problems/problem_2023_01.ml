let year = 2023
let day = 1

open! Base

let sum = List.fold_left ~init:0 ~f:( + )

type num_t = string * string list

let index_of line pattern =
  String.substr_index_all line ~pattern ~may_overlap:true

let pos_where line (num : num_t) =
  let num, patterns = num in
  List.concat_map patterns ~f:(index_of line)
  |> List.map ~f:(fun index -> (num, index))

let line_to_num nums line =
  let first_num2 =
    List.concat_map nums ~f:(pos_where line)
    |> List.min_elt ~compare:(fun a b ->
           match (a, b) with (_, a), (_, b) -> a - b)
  in
  let last_num2 =
    List.concat_map nums ~f:(pos_where line)
    |> List.max_elt ~compare:(fun a b ->
           match (a, b) with (_, a), (_, b) -> a - b)
  in

  match (first_num2, last_num2) with
  | None, None -> raise (Failure ("NO NUM ON LINE: " ^ line))
  | _, None -> raise (Failure ("ONLY FIRST NUM ON LINE: " ^ line))
  | None, _ -> raise (Failure ("ONLY LAST NUM ON LINE: " ^ line))
  | Some (first_num, _), Some (last_num, _) ->
      first_num ^ last_num |> Int.of_string

module Part_1 = struct
  let (nums : num_t list) =
    [
      ("1", [ "1" ]);
      ("2", [ "2" ]);
      ("3", [ "3" ]);
      ("4", [ "4" ]);
      ("5", [ "5" ]);
      ("6", [ "6" ]);
      ("7", [ "7" ]);
      ("8", [ "8" ]);
      ("9", [ "9" ]);
    ]

  let run (input : string) : (string, string) Stdlib.result =
    Ok
      (String.split_lines input
      |> List.map ~f:(line_to_num nums)
      |> sum
      |> Int.to_string)
end

module Part_2 = struct
  let (nums : num_t list) =
    [
      ("1", [ "1"; "one" ]);
      ("2", [ "2"; "two" ]);
      ("3", [ "3"; "three" ]);
      ("4", [ "4"; "four" ]);
      ("5", [ "5"; "five" ]);
      ("6", [ "6"; "six" ]);
      ("7", [ "7"; "seven" ]);
      ("8", [ "8"; "eight" ]);
      ("9", [ "9"; "nine" ]);
    ]

  let run (input : string) : (string, string) Stdlib.result =
    Ok
      (String.split_lines input
      |> List.map ~f:(line_to_num nums)
      |> sum
      |> Int.to_string)
end
