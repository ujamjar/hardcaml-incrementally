(*
  Perform cycle accurate simulation using Incremental as the scheduling engine.

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
  type simulator = register list * memory list
  type reset = unit -> simulator
  type cycle = simulator -> B.t I.t -> simulator * B.t O.t * B.t O.t

  exception Unexpected_signal of string
  exception Unsupported_signal of string
  exception Input_has_multiple_names of string

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

end = struct

  open Circuit
  open Signal.Types

  module Inc = Incremental_lib.Incremental.Make ()
  module Var = Inc.Var

  module MemMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

  type register = B.t
  type memory = B.t * B.t MemMap.t
  type simulator = register list * memory list
  type reset = unit -> simulator
  type cycle = simulator -> B.t I.t -> simulator * B.t O.t * B.t O.t

  exception Unexpected_signal of string
  exception Unsupported_signal of string
  exception Input_has_multiple_names of string

  let register_clr_ena map r = 
    let clr = 
      if r.reg_clear <> Signal.Comb.empty then 
        Some(UidMap.find (uid r.reg_clear) map,
             UidMap.find (uid r.reg_clear_value) map,
             const_value r.reg_clear_level = "1")
      else None 
    in
    let ena = 
      if r.reg_enable <> Signal.Comb.empty then
        Some(UidMap.find (uid r.reg_enable) map)
      else None 
    in
    clr, ena

  let bool_of_bit = Inc.map ~f:(fun d -> if B.to_int d = 0 then false else true) 

  type maps = 
    {
      (* interface hardware nodes *)
      comb : B.t Inc.t UidMap.t;
      (* input values *)
      inps : B.t Var.t UidMap.t;
      (* registers - signal, input Var.t and next Inc.t *)
      regs : (Signal.Comb.t * B.t Var.t * B.t Inc.t) UidMap.t;
      (* memories - signal, input Var.t and next Inc.t *)
      mems : (Signal.Comb.t * memory Var.t * memory Inc.t) UidMap.t;
    }

  let add_comb x y m = { m with comb = UidMap.add x y m.comb } 
  let add_inps x y m = { m with inps = UidMap.add x y m.inps } 
  let add_regs x y m = { m with regs = UidMap.add x y m.regs } 
  let add_mems x y m = { m with mems = UidMap.add x y m.mems } 

  let build_sim inputs outputs = 

    let unexpected signal = raise (Unexpected_signal (to_string signal)) in
    let unsupported signal = raise (Unsupported_signal (to_string signal)) in
    
    let opt = true in
    let poly x = if opt then (Inc.set_cutoff x Inc.Cutoff.poly_equal; x) else x in
    
    let deps_ne s = List.filter ((<>) Signal_empty) (deps s) in

    (* Construct incremental graph.
       Traverses the hardware graph recursively from outputs. *)
    let rec create map signal = 
      (*let () = Printf.printf "%s\n" (to_string signal) in*)
      match UidMap.find (uid signal) map.comb with
      | s -> map, s
      | exception Not_found -> begin
        match signal with

        | Signal_empty 
        | Signal_inst(_) -> unsupported signal

        (* constants *)
        | Signal_const(_) -> 
          let c = Inc.const (B.constb (const_value signal)) in
          add_comb (uid signal) c map, c

        (* operators ie (+:) etc *)
        | Signal_op(_,op) -> begin
          let map, deps = create_list map (deps signal) in
          let add i = add_comb (uid signal) i map, i in
          let op2 op = 
            let a = List.nth deps 0 in
            let b = List.nth deps 1 in
            add @@ Inc.map2 ~f:op a b
          in 
          match op with
          | Signal_add -> op2 B.(+:) 
          | Signal_sub -> op2 B.(-:) 
          | Signal_mulu -> op2 B.( *: ) 
          | Signal_muls -> op2 B.( *+ )
          | Signal_and -> op2 B.(&:)
          | Signal_or -> op2 B.(|:)
          | Signal_xor -> op2 B.(^:)
          | Signal_eq -> op2 B.(==:)
          | Signal_not -> add @@ Inc.map ~f:B.(~:) (List.hd deps)
          | Signal_lt -> op2 B.(<:)
          | Signal_cat -> add @@ Inc.map ~f:B.concat Inc.(all deps)
          | Signal_mux -> 
            let sel = List.hd deps in
            let data = List.tl deps in
            let last = List.length data - 1 in
            (*add @@ Inc.map2 
              ~f:(fun sel data -> List.nth data (min last (B.to_int sel))) 
              sel Inc.(all data)*)
            let data = Array.of_list data in
            add @@ Inc.bind sel (fun sel -> data.(min last (B.to_int sel)))
        end

        (* input wire *)
        | Signal_wire(_,d) when !d = Signal_empty -> 
          if List.length (names signal) <> 1 then 
            raise (Input_has_multiple_names (to_string signal))
          else
            let v = Var.create @@ B.zero (width signal) in (* XXX need this value *)
            let map = add_inps (uid signal) v map in
            let w = poly @@ Var.watch v in
            add_comb (uid signal) w map, w

        (* internal wires *)
        | Signal_wire(_,d) -> 
          let map, d = create map !d in
          let d = Inc.map ~f:(fun x -> x) d in
          add_comb (uid signal) d map, d

        (* part selection *)
        | Signal_select(_,h,l) -> 
          let map, s = create map (List.hd (deps signal)) in
          let s = Inc.map ~f:(fun s -> B.select s h l) s in
          add_comb (uid signal) s map, s

        (* registers *)
        | Signal_reg(_,r) ->
          (* create input for register, add to map *)
          let v = Var.create @@ B.zero (width signal) in
          let q = poly @@ Var.watch v in
          let map = add_comb (uid signal) q map in
          (* recurse to inputs (data, clock, reset, clear etc) *)
          let map, d = create_list map (deps_ne signal) in
          let d = List.hd d in (* input data *)
          (* implement next value register logic *)
          let clr, ena = register_clr_ena map.comb r in
          let lev l b = if l then b else Inc.map ~f:(not) b in
          let d = 
            match ena with 
            | Some(e) -> Inc.if_ (bool_of_bit e) ~then_:d ~else_:q 
            | None -> d 
          in
          let d = 
            match clr with 
            | Some(c,v,l) -> Inc.if_ (lev l (bool_of_bit c)) ~then_:v ~else_:d
            | None -> d
          in
          (* add input and next reg value *)
          let map = add_regs (uid signal) (signal,v,d) map in
          map, q

        (* memories *)
        | Signal_mem(_,_,r,ms) -> 
          let map, ra = create map ms.mem_read_address in (* XXX mustnt be a comb loop from q->ra *)
          let v = Var.create @@ (B.zero (width signal), (MemMap.empty : B.t MemMap.t)) in
          let m = Var.watch v in
          let q = 
            poly @@ Inc.map2 
              ~f:(fun addr (const,mem) -> 
                    try MemMap.find (B.to_int addr) mem with Not_found -> const) ra m
          in
          let map = add_comb (uid signal) q map in
          let map, _ = create_list map (deps_ne signal) in
          (* memory write logic (clear, enable etc) *)
          let clr, ena = register_clr_ena map.comb r in
          let lev l b = if l then b else Inc.map ~f:(not) b in
          let m = 
            match ena with 
            | Some(e) -> 
              let d = UidMap.find (uid (List.hd (deps signal))) map.comb in
              let wa = UidMap.find (uid ms.mem_write_address) map.comb in
              Inc.if_ (bool_of_bit e) 
                ~then_:Inc.(map3 ~f:(fun wa d (c,m) -> c, MemMap.add (B.to_int wa) d m) wa d m)
                ~else_:m 
            | None -> m 
          in
          let m = 
            match clr with 
            | Some(c,v,l) -> 
              Inc.if_ (lev l (bool_of_bit c)) 
                ~then_:Inc.(map2 ~f:(fun (_,m) c -> c, MemMap.empty) m v) 
                ~else_:m
            | None -> m
          in
          (* add input and next mem value *)
          let map = add_mems (uid signal) (signal,v,m) map in
          map, q

      end 

    and create_list map signals = 
      let map, l = 
        List.fold_left 
          (fun (map,l) s -> 
            let map,s = create map s in
            map, s::l) (map,[]) signals
      in
      map, List.rev l
    in

    (* traverse the design from all outputs *)
    let empty_maps =
      {
        comb = UidMap.empty;
        inps = UidMap.empty;
        regs = UidMap.empty;
        mems = UidMap.empty;
      }
    in
    let imap, _ = create_list empty_maps (O.to_list outputs) in

    (* outputs *)
    let outputs_o = O.(map (fun s -> Inc.observe @@ UidMap.find (uid s) imap.comb) outputs) in

    (* registers *)
    let regs_t, regs_i, regs_o = 
      let regs = UidMap.bindings imap.regs in
      List.map (fun (_,(s,_,_)) -> s) regs,
      List.map (fun (_,(_,v,_)) -> v) regs,
      List.map (fun (_,(_,_,w)) -> Inc.observe w) regs 
    in

    (* memories *)
    let mems_t, mems_i, mems_o = 
      let mems = UidMap.bindings imap.mems in
      List.map (fun (_,(s,_,_)) -> s) mems,
      List.map (fun (_,(_,v,_)) -> v) mems,
      List.map (fun (_,(_,_,w)) -> Inc.observe w) mems
    in

    (* inputs - convert map to I.t interface record *)
    let inputs_v = UidMap.bindings imap.inps in
    let inputs_i = 
      I.(map (fun i -> try List.assoc (uid i) inputs_v 
                       with _ -> Var.create (B.zero (width i))) inputs) 
    in

    let reset () = 
      (* get the registers reset value (or 0 if not specified) *)
      let reset_reg s = 
        match s with
        | Signal_reg(_,r) | Signal_mem(_,_,r,_) -> begin
           let rst_value = r.reg_reset_value in
           try B.constb (const_value rst_value) 
           with _ -> B.zero (width s)
        end
        | _ -> unexpected s
      in
      let reset_mem s = reset_reg s, MemMap.empty in
      List.map reset_reg regs_t, List.map reset_mem mems_t
    in

    let cycle (regs_s, mems_s) inps = 
      (* apply inputs *)
      ignore @@ I.(map2 Var.set inputs_i inps);
      (* apply current register values *)
      List.iter2 Var.set regs_i regs_s;
      List.iter2 Var.set mems_i mems_s;
      (* compute register/memory inputs *)
      Inc.stabilize ();
      let outputs_c = O.map Inc.Observer.value_exn outputs_o in
      (* get updated register/memory values *)
      let regs_n = List.map Inc.Observer.value_exn regs_o in
      let mems_n = List.map Inc.Observer.value_exn mems_o in
      (* reapply register values, and compute outputs *)
      List.iter2 Var.set regs_i regs_n;
      List.iter2 Var.set mems_i mems_n;
      Inc.stabilize ();
      let outputs_n = O.map Inc.Observer.value_exn outputs_o in
      (* next state and outputs *)
      (regs_n, mems_n), outputs_c, outputs_n
    in

    reset, cycle

  (* construct simulator *)
  let make f = 
    let i = I.map (fun (n,b) -> Signal.Comb.input n b) I.t in
    let o = f i in
    let reset, cycle = build_sim i o in
    reset, cycle

  module Stats = struct

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

    let get () = 
      let open Inc.State in
      {
        stabilizes = num_stabilizes t;
        var_sets = num_var_sets t;
        necessary = num_nodes_became_necessary t;
        unnecessary = num_nodes_became_unnecessary t;
        changed = num_nodes_changed t;
        created = num_nodes_created t;
        recomputed = num_nodes_recomputed t;
      }

    let diff s t = 
      {
        stabilizes = s.stabilizes - t.stabilizes;
        var_sets = s.var_sets - t.var_sets;
        necessary = s.necessary - t.necessary;
        unnecessary = s.unnecessary - t.unnecessary;
        changed = s.changed - t.changed;
        created = s.created - t.created;
        recomputed = s.recomputed - t.recomputed;
      }

    let print f stats = 
      Printf.fprintf f "[%i] sets=%i necessary=+%i/-%i node=%i/%i/%i\n"
        stats.stabilizes stats.var_sets stats.necessary stats.unnecessary
        stats.changed stats.created stats.recomputed

    let print_diff f = 
      let s = ref (get ()) in
      (fun () -> 
         let t = get () in
         print f (diff t !s);
         s := t)

  end

end


