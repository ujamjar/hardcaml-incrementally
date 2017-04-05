(** {2 Perform cycle accurate simulation using Incremental as the scheduling engine.}

  The combinatorial logic is converted to an Incremental DAG by extracting all 
  registers and turning then into input/output pairs.

  (note; the API doesn't enforce combinatorial logic to be a DAG, but it doesn't
         make sense if its not.  With registers, the graph can be cyclic which is
         why they must be extracted)

  Register and memory state is updated externally.

  An update step consists of the following parts

   1. Inputs are set by the testbench
   2. The inputs are copied into the incremental graph.
   3. Current values of registers are copied to the incremental graph
   4. The graph is stabalized - this calculates the next input values for the 
      registers (and the output values, based on the new inputs and the old
      register state).

  The above does the calculations required and updates correctly, however, it
  doesn't produce the correct outputs.  The following additional steps are
  therefore (sometimes) required

   5. Update the register inputs with the newly calculated values
   6. Stabalize the graph

  (note; step 3 can be moved into the reset function)
*)
open HardCaml

module Make(B : Comb.S)(I : Interface.S)(O : Interface.S)() : sig

  module MemMap : Map.S with type key = int

  type register = B.t
  type memory = B.t * B.t MemMap.t
  type state = register list * memory list

  type reset = unit -> state
  type cycle = state -> B.t I.t -> state * B.t O.t * B.t O.t

  type simfns = 
    {
      init : unit -> state;
      set_inputs : B.t I.t -> unit;
      set_state : state -> unit;
      get_outputs : unit -> B.t O.t;
      get_state : unit -> state;
      stabilize : unit -> unit;
    }

  exception Unexpected_signal of string
  exception Unsupported_signal of string
  exception Input_has_multiple_names of string

  (* simulator construction.  returns a record with various operations for controlling
     aspects of the simulation *)
  val make_inc : (Signal.Comb.t I.t -> Signal.Comb.t O.t) -> simfns

  (* various sub-parts of a cycle compatible with the original simulator *)
  val reset : simfns -> reset
  val cycle_comb0 : simfns -> state -> B.t I.t -> B.t O.t
  val cycle_seq : simfns -> state
  val cycle_comb1 : simfns -> B.t O.t
  val cycle : simfns -> cycle

  (* simulator with a simple functional interface *)
  val make : (Signal.Comb.t I.t -> Signal.Comb.t O.t) -> reset * cycle

  module Stats : sig
    type t = 
      {
        stabilizes : int;
        var_sets : int;
        necessary : int;
        unnecessary : int;
        changed : int;
        created : int;
        recomputed : int;
      }

    val get : unit -> t
    val diff : t -> t -> t
    val print : out_channel -> t -> unit
    val print_diff : out_channel -> (unit -> unit)
  end

end 

