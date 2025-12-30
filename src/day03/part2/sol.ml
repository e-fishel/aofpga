(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

open! Core
open! Hardcaml
open! Signal

let num_digits = 2
let base = 10
let base_bits = Int.ceil_log2 (base + 1)
let num_lines = 200
let lines_bits = Int.ceil_log2 (num_lines + 1)
let in_width = base_bits
let mid_width = num_digits * base_bits
let out_width = num_digits * base_bits + lines_bits

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
    { ans : 'a With_valid.t [@bits out_width]
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
    State_machine.create (module States) spec
  in
  let ve = Array.init num_digits ~f:(fun i ->
    let r = Variable.reg spec ~width:in_width in
    ignore (Scope.naming scope r.value (Printf.sprintf "ve_%d" i));
    r
  ) in

  let%hw_var my_ans = Variable.reg spec ~width:out_width in
  let%hw_var pans = Variable.wire ~default:(zero mid_width) () in
  let ans_valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ 
              when_
                start
                ((Array.map ve ~f:(fun x -> x <-- zero in_width) |> Array.to_list)
                @ [
                  sm.set_next Accepting_inputs
                ])
            ] )
        ; ( Accepting_inputs
          , [ 
              (* pans <-- (ve.(1).value *: of_int_trunc ~width:base_factor base) +: (zero base_factor @: ve.(0).value) *)
              when_ data_in_valid
              [ if_ (ve.(0).value >: ve.(1).value)
                [
                  ve.(1) <-- ve.(0).value
                ; ve.(0) <-- data_in
                ] (*else*) [
                  when_ (data_in >: ve.(0).value)
                  [
                    ve.(0) <-- data_in
                  ]
                ]
              (* ; when_ (data_in >: max.value) [ max <-- data_in ] *)
              ]
            ; when_ finish_line [
                pans <-- Array.fold ~init:ve.(0).value (Array.sub ~pos:1 ~len:(num_digits-1) ve) ~f:(fun t i ->
                  (zero base_bits @: t) +: (i.value *: of_int_trunc ~width:base_bits base)
                )
              ; my_ans <-- my_ans.value +: (zero (out_width - mid_width) @: pans.value)
              ; sm.set_next Done_line
              ]
            ] )
        ; ( Done_line
          , [ 
              when_ start
              ((Array.map ve ~f:(fun x -> x <-- zero in_width) |> Array.to_list)
              @ [
                sm.set_next Accepting_inputs
              ])
            ; when_ finish [
                sm.set_next Done
              ]
            ] )
        ; ( Done
          , [
              ans_valid <-- vdd
            ] )
        ]
    ];
  { ans = { value = my_ans.value; valid = ans_valid.value }; state = sm.current }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"sol" create
