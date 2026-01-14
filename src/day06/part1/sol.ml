(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

open! Core
open! Hardcaml
open! Signal

let max_x = 10_000
let max_x_bits = Int.ceil_log2 (max_x + 1)
let num_lines = 4
let lines_bits = Int.ceil_log2 (num_lines + 1)
let num_cols = 1_000
let cols_bits = Int.ceil_log2 (num_cols + 1)
let ram_size = Int.pow 2 cols_bits
let in_width = max_x_bits
let mid_width = max_x_bits * num_lines
let out_width = cols_bits + max_x_bits * num_lines (* log_2(num_cols * max_n ^ num_lines) *)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits in_width]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ans : 'a With_valid.t [@bits out_width]
    ; state : 'a [@bits 3]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Idle_nums
    | Processing_nums
    | Done_nums
    | Idle_ops
    | Accepting_ops
    | Done_ops
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm =
    State_machine.create (module States) spec
  in

  (* let sums = Array.init num_cols ~f:(fun i ->
    let r = Variable.reg spec ~width:mid_width in
    (* ignore (Scope.naming scope r.value (Printf.sprintf "sums_%i" i)); *)
    (* TOGGLE OFF FOR NAMING *)
    r
  ) in *)
  (* let prods = Array.init num_cols ~f:(fun i ->
    let r = Variable.reg spec ~width:mid_width in
    (* ignore (Scope.naming scope r.value (Printf.sprintf "prods_%i" i)); *)
    (* TOGGLE OFF FOR NAMING *)
    r
  ) in *)

  let%hw_var sum_write_enable = Variable.reg spec ~enable:vdd ~width:1 in
  let%hw_var sum_write_address = Variable.reg spec ~enable:vdd ~width:cols_bits in
  let%hw_var sum_write_data = Variable.wire ~default:(zero mid_width) () in
  let%hw_var sum_read_enable = Variable.reg spec ~enable:vdd ~width:1 in
  let%hw_var sum_read_address = Variable.reg spec ~enable:vdd ~width:cols_bits in
  let ram_sum = Ram.create
    ~name:"ram_sum"
    ~collision_mode:Ram.Collision_mode.Read_before_write
    ~size:ram_size
    ~write_ports:[|{
      write_clock = clock;
      write_enable = sum_write_enable.value;
      write_address = sum_write_address.value;
      write_data = sum_write_data.value;
    }|]
    ~read_ports:[|{
      read_clock = clock;
      read_enable = sum_read_enable.value;
      read_address = sum_read_address.value;
    }|] ()
  in
  let%hw sum_read_data = ram_sum.(0) in

  let%hw_var prod_write_enable = Variable.reg spec ~enable:vdd ~width:1 in
  let%hw_var prod_write_address = Variable.reg spec ~enable:vdd ~width:cols_bits in
  let%hw_var prod_write_data = Variable.wire ~default:(zero mid_width) () in
  let%hw_var prod_read_enable = Variable.reg spec ~enable:vdd ~width:1 in
  let%hw_var prod_read_address = Variable.reg spec ~enable:vdd ~width:cols_bits in
  let ram_prod = Ram.create
    ~name:"ram_prod"
    ~collision_mode:Ram.Collision_mode.Read_before_write
    ~size:ram_size
    ~write_ports:[|{
      write_clock = clock;
      write_enable = prod_write_enable.value;
      write_address = prod_write_address.value;
      write_data = prod_write_data.value;
    }|]
    ~read_ports:[|{
      read_clock = clock;
      read_enable = prod_read_enable.value;
      read_address = prod_read_address.value;
    }|] ()
  in
  let%hw prod_read_data = wire mid_width in
  Signal.(prod_read_data <-- mux2 (ram_prod.(0) ==:. 0) (of_int_trunc ~width:mid_width 1) ram_prod.(0));
  (* if ram reads out 0 (would only happen in the first cycle because there are no 0s in data) then return 1, otherwise return the ram's value *)

  let%hw_var data_in_dd = Variable.reg spec ~width:in_width in
  let%hw_var data_in_d = Variable.reg spec ~width:in_width in
  let%hw_var cur_col = Variable.reg spec ~width:cols_bits in

  let%hw_var ans_adds_from_sum_next = Variable.reg spec ~width:1 in
  let%hw_var ans_adds_from_prod_next = Variable.reg spec ~width:1 in
  
  let ans = Variable.reg spec ~width:out_width in
  let ans_valid = Variable.reg spec ~width:1 in

  (* ignore data_in_d;
  ignore cur_col;
  ignore sum_read_data;
  ignore start;
  ignore finish;
  ignore data_in;
  ignore data_in_valid; *)

  compile
    [ sm.switch
        [ ( Idle
          , [ when_ start
              [ sm.set_next Idle_nums
              ; ans <-- zero out_width
              ; ans_valid <-- gnd
              ]
            ] )
        ; ( Idle_nums
          , [ when_ start
              [ cur_col <-- zero cols_bits
              ; sm.set_next Processing_nums
              (* sum ram already starts in 0s and prod ram starts in the value 1 (thanks to our way of reading) *)
              ]
            ; when_ finish
              [ sm.set_next Idle_ops
              ]
            ] )
        ; ( Processing_nums
          , [ when_ sum_write_enable.value
              [ sum_write_enable <-- gnd
              ; sum_write_data <-- sum_read_data +: ((zero (mid_width - in_width)) @: data_in_dd.value)
              ]
            ; when_ sum_read_enable.value
              [ sum_read_enable <-- gnd
              ; sum_write_address <-- sum_read_address.value (* not necessarily cur_col (it will have advanced by 1) *)
              ; sum_write_enable <-- vdd
              ]
            ; when_ prod_write_enable.value
              [ prod_write_enable <-- gnd
              ; prod_write_data <-- ((prod_read_data *: data_in_dd.value) |> sel_bottom ~width:mid_width)
              ]
            ; when_ prod_read_enable.value
              [ prod_read_enable <-- gnd
              ; prod_write_address <-- prod_read_address.value (* not necessarily cur_col (it will have advanced by 1) *)
              ; prod_write_enable <-- vdd
              ; data_in_dd <-- data_in_d.value
              ]
            ; when_ data_in_valid
              [ data_in_d <-- data_in
              ; sum_read_address <-- cur_col.value
              ; prod_read_address <-- cur_col.value
              ; cur_col <-- cur_col.value +:. 1
              ; sum_read_enable <-- vdd
              ; prod_read_enable <-- vdd
              ]
            ; when_ finish
              [ sm.set_next Idle_nums
              ]
            ] )
        ; ( Done_nums
          , [ when_ finish
              [ sm.set_next Idle_ops
              ]
            ] )
        ; ( Idle_ops
          , [ when_ start
              [ cur_col <-- zero cols_bits
              ; sm.set_next Accepting_ops
              ]
            ] )
        ; ( Accepting_ops
          , [ when_ ans_adds_from_sum_next.value
              [ ans_adds_from_sum_next <-- gnd
              ; ans <-- ans.value +: ((zero (out_width - mid_width)) @: sum_read_data)
              ]
            ; when_ ans_adds_from_prod_next.value
              [ ans_adds_from_prod_next <-- gnd
              ; ans <-- ans.value +: ((zero (out_width - mid_width)) @: prod_read_data)
              ]
            ; when_ sum_read_enable.value
              [ sum_read_enable <-- gnd
              ; ans_adds_from_sum_next <-- vdd
              ]
              ; when_ prod_read_enable.value
              [ prod_read_enable <-- gnd
              ; ans_adds_from_prod_next <-- vdd
              ]
            ; when_ data_in_valid
              [ cur_col <-- cur_col.value +:. 1
              ; when_ (data_in ==:. 43) (* char code of + sign is 43 *)
                [ sum_read_address <-- cur_col.value
                ; sum_read_enable <-- vdd
                ]
              ; when_ (data_in ==:. 42) (* char code of * sign is 42 *)
                [ prod_read_address <-- cur_col.value
                ; prod_read_enable <-- vdd
                ]
              ]
            ; when_ finish
              [ sm.set_next Done_ops
              ]
            ] )
        ; ( Done_ops
          , [ when_ finish
              [ sm.set_next Done
              ]
            ] )
        ; ( Done
          , [ ans_valid <-- vdd
            ] )
        ]
    ];
  { ans = { value = ans.value; valid = ans_valid.value }; state = sm.current }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"sol" create
