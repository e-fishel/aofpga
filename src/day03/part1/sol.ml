(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal

let num_digits = 2
let base = 10
let base_factor = 1 + Int.floor_log2 base
let num_lines = 200
let lines_factor = 1 + Int.floor_log2 num_lines
let in_width = base_factor
let mid_width = num_digits * base_factor
let out_width = num_digits * base_factor + lines_factor

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish_line : 'a
    ; finish : 'a
    ; data_in : 'a [@bits in_width]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      ans : 'a With_valid.t [@bits out_width]
    ; state : 'a [@bits 2]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done_line
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; finish_line; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    (* Note that the state machine defaults to initializing to the first state *)
    State_machine.create (module States) spec
  in
  (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
     will show up in waveforms. The [_var] version is used when working with the Always
     DSL. *)
  let%hw_var dig0 = Variable.reg spec ~width:in_width in
  let%hw_var dig1 = Variable.reg spec ~width:in_width in
  (* We don't need to name the range here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
  let%hw_var my_ans = Variable.reg spec ~width:out_width in
  let%hw_var pans = Variable.wire ~default:(zero mid_width) () in
  let ans_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ 
              when_
                start
                [ dig0 <-- zero in_width
                ; dig1 <-- zero in_width
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ 
              (* pans <-- (dig1.value *: of_int_trunc ~width:base_factor base) +: (zero base_factor @: dig0.value) *)
              when_ data_in_valid
              [ if_ (dig0.value >: dig1.value)
                [
                  dig1 <-- dig0.value
                ; dig0 <-- data_in
                ] (*else*) [
                  when_ (data_in >: dig0.value)
                  [
                    dig0 <-- data_in
                  ]
                ]
              (* ; when_ (data_in >: max.value) [ max <-- data_in ] *)
              ]
            ; when_ finish_line [
                pans <-- (dig1.value *: of_int_trunc ~width:base_factor base) +: (zero base_factor @: dig0.value)
              ; my_ans <-- my_ans.value +: (zero (out_width - mid_width) @: pans.value)
              ; sm.set_next Done_line
              ]
            ] )
        ; ( Done_line
          , [ 
              when_ start
              [
                dig0 <-- zero in_width
              ; dig1 <-- zero in_width
              ; sm.set_next Accepting_inputs
              ]
            ; when_ finish [
                sm.set_next Done
              ]
            ] )
        ; ( Done
          , [
              ans_valid <-- vdd
            (* ; when_ finish [ sm.set_next Accepting_inputs ] *)
            ] )
        ]
    ];
  (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
  { ans = { value = my_ans.value; valid = ans_valid.value }; state = sm.current }

(* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"sol" create
