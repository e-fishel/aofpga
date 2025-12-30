module type Parse_t = sig
  type t
  val parse : string -> t
  val parsed_example_input : t
  val parsed_input : t
end

module type Raw_t = sig
  val raw_example_input : string
  val raw_input : string
end