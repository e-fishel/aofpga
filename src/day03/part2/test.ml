open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Harness = Cyclesim_harness.Make (Sol.I) (Sol.O)

let ( <--. ) = Bits.( <--. )

open Aoc_input.Parse03

(* let parsed_example_input = Parsed_inputs.parsed_example_input *)
let parsed_input = Parsed_inputs.parsed_input

let simple_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper function for inputting one value *)
  let feed_digit (n : int) : unit =
    inputs.data_in <--. n;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  let feed_line (s : int list) : unit =
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    List.iter s ~f:feed_digit;
    inputs.finish_line := Bits.vdd;
    cycle ();
    inputs.finish_line := Bits.gnd
  in
  let feed_input (s : int list list) : unit =
    List.iter s ~f:feed_line;
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();

  (* Input some data *)
  feed_input parsed_input;
  
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.ans.valid)) do
    cycle ()
  done;
  let ans = Bits.to_unsigned_int !(outputs.ans.value) in
  print_s [%message "Result" (ans : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()



let () =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "sol*" |> Re.compile)
    ] @ (
      let output_rules =
        { Sol.O.(map port_names ~f:(Display_rule.port_name_is ~wave_format:(Bit_or Unsigned_int))) with
          Sol.O.state =
            Display_rule.port_name_is
              "state"
              ~wave_format:
                (Index
                    (List.map Sol.States.all ~f:(fun t -> Sol.States.sexp_of_t t |> Sexp.to_string)))
        }
        |> Sol.O.to_list
      in
      output_rules
    )
  in
  Harness.run_advanced
    ~create:Sol.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:192
        ~wave_width:1
        waves)
    simple_testbench

let () =
  let waves_config = Waves_config.no_waves in
  Harness.run_advanced ~waves_config ~create:Sol.hierarchical simple_testbench