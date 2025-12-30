open Core
open Raw03

module Parsed_inputs = struct
  type t = int list list

  let parse input =
    input
    |> String.split_on_chars ~on:[ '\n' ]
    |> List.map ~f:(fun s ->
      s
      |> String.to_list
      |> List.map ~f:Char.get_digit_exn
    )

  let parsed_example_input =
    parse Raw_inputs.raw_example_input

  let parsed_input =
    parse Raw_inputs.raw_input
end