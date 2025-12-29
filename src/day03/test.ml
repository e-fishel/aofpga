open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Harness = Cyclesim_harness.Make (Sol.I) (Sol.O)

let ( <--. ) = Bits.( <--. )

let parsed_input = Aoc_input.parsed_input

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
  (* while not (Bits.to_bool !(outputs.ans.valid)) do
    cycle ()
  done; *)
  let ans = Bits.to_unsigned_int !(outputs.ans.value) in
  print_s [%message "Result" (ans : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
   for viewing later with a waveform viewer. The commented examples below show how to save
   a waveterm file or a VCD file. *)
let waves_config = Waves_config.no_waves

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

(* let%expect_test "Simple test, optionally saving waveforms to disk" = *)
let () =
  Harness.run_advanced ~waves_config ~create:Sol.hierarchical simple_testbench;
  (* [%expect {| (Result (range 146)) |}] *)
;;

(* let%expect_test "Simple test with printing waveforms directly" = *)
let () =
  (* For simple tests, we can print the waveforms directly in an expect-test (and use the
     command [dune promote] to update it after the tests run). This is useful for quickly
     visualizing or documenting a simple circuit, but limits the amount of data that can
     be shown. *)
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "sol*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Sol.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:192
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    simple_testbench;
  (* [%expect
    {|
    (Result (range 146))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────┐
    │range_finder$i$clear        ││────┐                                                       │
    │                            ││    └───────────────────────────────────────────────────────│
    │range_finder$i$clock        ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───────┬───────┬───────┬───────────────────────│
    │range_finder$i$data_in      ││ 0          │16     │67     │150    │4                      │
    │                            ││────────────┴───────┴───────┴───────┴───────────────────────│
    │range_finder$i$data_in_valid││            ┌───┐   ┌───┐   ┌───┐   ┌───┐                   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───────────────────│
    │range_finder$i$finish       ││                                            ┌───┐           │
    │                            ││────────────────────────────────────────────┘   └───────────│
    │range_finder$i$start        ││        ┌───┐                                               │
    │                            ││────────┘   └───────────────────────────────────────────────│
    │                            ││────────────────┬───────┬───────┬───────────────────────────│
    │range_finder$max            ││ 0              │16     │67     │150                        │
    │                            ││────────────────┴───────┴───────┴───────────────────────────│
    │                            ││────────────┬───┬───────────────────────┬───────────────────│
    │range_finder$min            ││ 0          │65.│16                     │4                  │
    │                            ││────────────┴───┴───────────────────────┴───────────────────│
    │range_finder$o$range$valid  ││                                                ┌───────────│
    │                            ││────────────────────────────────────────────────┘           │
    │                            ││────────────────────────────────────────────────┬───────────│
    │range_finder$o$range$value  ││ 0                                              │146        │
    │                            ││────────────────────────────────────────────────┴───────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────┘
    |}] *)
;;
