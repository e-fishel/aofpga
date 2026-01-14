open Core
open Raw06

module Parsed_inputs = struct
  type t = int list list * char list

  let parse input =
    let parse_num_line line =
      let number_regex = Re.Perl.re "\\d+" |> Re.Perl.compile in
      Re.matches number_regex line
      |> List.map ~f:int_of_string
    in
    let parse_op_line line =
      let operator_regex = Re.Perl.re "[+*]" |> Re.Perl.compile in
      Re.matches operator_regex line
      |> List.map ~f:(fun i -> String.get i 0)
      (* or |> List.map ~f:((Fun.flip String.get) 0) *)
    in
    let input' =
      input
      |> String.split_on_chars ~on:[ '\n' ]
      |> List.rev
    in
    let (raw_operators, raw_numbers) =
      match input' with
      | a :: b -> a, List.rev b
      | _ -> failwith "need at least 2 lines in a file"
    in
    let parsed_operators = parse_op_line raw_operators in
    let parsed_numbers = List.map raw_numbers ~f:parse_num_line in
    (parsed_numbers, parsed_operators)
    (* |> List.mapi ~f:(fun j i ->
      match j with
      | 0 -> String.to_list
      | List.map ~f:Char.get_digit_exn
    )
    |> List.rev *)

  let parsed_example_input =
    parse Raw_inputs.raw_example_input

  let parsed_input =
    parse Raw_inputs.raw_input
end