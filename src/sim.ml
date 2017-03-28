(*
  Perform cycle accurate simulation using Incremental as the scheduling engine.

  The combinatorial logic is converted to an Incremental DAG by extracting all 
  registers and turning then into input/output pairs.

  (note; the API doesn't enforce combinatorial logic to be a DAG, but it doesn't
         make sense if its not.  With registers, the graph can be acyclic which is
         why they must be extracted)

  Register state is updated externally.

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

  TODO;

   * Implement the mux and concat nodes (how to go from ['a Inc.t list] to ['a list Inc.t])
   * Implement the clear/enable logic around registers (done)
   * How do we deal with memories, in particular the read_address?

*)
open HardCaml

module Make(B : Comb.S)(I : Interface.S)(O : Interface.S)() : sig

  type simulator
  type reset = unit -> simulator
  type cycle = simulator -> B.t I.t -> simulator * B.t O.t

  val make : (Signal.Comb.t I.t -> Signal.Comb.t O.t) -> reset * cycle

end = struct

  open Circuit
  open Signal.Types

  module Inc = Incremental_lib.Incremental.Make ()
  module Var = Inc.Var

  type simulator = B.t list
  type reset = unit -> simulator
  type cycle = simulator -> B.t I.t -> simulator * B.t O.t

  let is_input signal = 
    match signal with
    | Signal_wire(_,d) when !d = Signal_empty && List.length (names signal) = 1 -> true
    | _ -> false

  let find_elements outputs =
    search
      (fun (regs, mems, consts, inputs, remaining) signal ->
         if signal = Signal.Comb.empty then
           (regs, mems, consts, inputs, remaining)
         else if is_reg signal then
           (signal::regs, mems, consts, inputs ,remaining)
         else if is_const signal then
           (regs, mems, signal::consts, inputs ,remaining)
         else if is_input signal then
           (regs, mems, consts, signal::inputs ,remaining)
         else if is_mem signal then
           (regs, signal::mems, consts, inputs, remaining)
         else
           (regs, mems, consts, inputs, signal::remaining)
      ) id ([],[],[],[],[]) outputs 

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

  let build_sim inputs outputs = 

    (* set up initial incremental nodes for inputs, constants and registers *)
    let regs, mems, consts, inputs_l, remaining = find_elements (O.to_list outputs) in

    (* create inputs vars
       
       We *should* have the same inputs defined in the interface, as found in the
       circuit.  The following deals with problems if they dont exactly match. *)
    let inputs_v, inputs_w, inputs_lvw = 
      let mk b = 
        let v = Var.create @@ B.zero b in
        let w = Var.watch v in
        v, w
      in
      let l = List.map (fun s -> List.hd (names s), mk (width s)) inputs_l in
      let i = I.map (fun (n,b) -> try List.assoc n l with Not_found -> mk b) I.t in
      I.map fst i, I.map snd i, l
    in

    (* create vars for the current value of each register *)
    let regs_v = List.map (fun s -> Var.create @@ B.zero (width s)) regs in
    let regs_w = List.map Var.watch regs_v in

    (* add inputs, constants and registers to the initial map *)
    let imap = UidMap.empty in
    let imap = List.fold_left2 (fun m r i -> UidMap.add (uid r) i m) imap regs regs_w in
    let imap = List.fold_left 
        (fun m c -> UidMap.add (uid c) (Inc.const @@ B.constb @@ const_value c) m) 
        imap consts 
    in
    let imap = 
      List.fold_left2 (fun m s (_,(_,v)) -> UidMap.add (uid s) v m)
        imap inputs_l inputs_lvw 
    in

    (* construct incremental graph *)
    let rec create map signal = 
      (*let () = Printf.printf "%s\n" (to_string signal) in*)
      match UidMap.find (uid signal) map with
      | s -> map, s
      | exception Not_found -> begin
        let map, deps = 
          List.fold_left 
            (fun (map,l) s -> 
              let map,s = create map s in
              map, s::l) (map,[]) (deps signal)
        in
        let deps = List.rev deps in
        let add i = UidMap.add (uid signal) i map, i in
        match signal with
        | Signal_empty -> 
          failwith "cant compile empty signal"
        | Signal_inst(_,_,i) -> 
          failwith ("Instantiation " ^ i.inst_name ^ " not supported in simulation")
        | Signal_const(_) 
        | Signal_reg(_) ->
          failwith "signal expected to be in map"
        | Signal_mem(_) -> 
          failwith "not sure how to deal with memories..."
        | Signal_op(_,op) ->
          begin
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
            (* XXX TODO *)
            | Signal_cat -> failwith "concat not implemented"
            | Signal_mux -> failwith "mux not implemented"
          end
        | Signal_wire(_,d) -> 
          add @@ Inc.map ~f:(fun x -> x) (List.hd deps) 
        | Signal_select(_,h,l) -> add @@ Inc.map ~f:(fun s -> B.select s h l) (List.hd deps)
      end 
    in

    (* traverse the design from all outputs *)
    let create_list map lst = List.fold_left (fun map s -> fst @@ create map s) map lst in
    let imap = create_list imap (O.to_list outputs) in
    (* and from the dependants of registers *)
    let imap = List.fold_left (fun map s -> create_list map (deps s)) imap regs in

    (* next value of each register also implementing clear and enable logic *)
    let create_reg (map : B.t Inc.t UidMap.t) signal = 
      match signal with
      | Signal_reg(_,r) -> begin
        let d = UidMap.find (uid (List.hd (deps signal))) map in
        let q = UidMap.find (uid signal) map in
        let clr, ena = register_clr_ena map r in
        let lev l b = if l then b else Inc.map ~f:(not) b in
        let d = 
          match ena with 
          | Some(e) -> Inc.if_ (bool_of_bit e) ~then_:d ~else_:q 
          | None -> d 
        in
        match clr with 
        | Some(c,v,l) -> Inc.if_ (lev l (bool_of_bit c)) ~then_:v ~else_:d
        | None -> d
      end
      | _ -> failwith "expecting reg"
    in
    let regs_nxt = List.map (fun s -> create_reg imap s) regs in

    let reset () = 
      (* get the registers reset value (or 0 if not specified) *)
      let reset_val s = 
        match s with
        | Signal_reg(id,r) -> begin
           let rst_value = r.reg_reset_value in
           try B.constb (const_value rst_value) 
           with _ -> B.zero id.s_width 
        end
        | _ -> failwith "unexpected"
      in
      List.map reset_val regs 
    in

    (* observed outputs and registers *)
    let outputs_o = O.(map (fun s -> Inc.observe @@ UidMap.find (uid s) imap) outputs) in
    let regs_o = List.map Inc.observe regs_nxt in

    let cycle state inps = 
      (* apply inputs *)
      ignore @@ I.(map2 Var.set inputs_v inps);
      (* apply current register values *)
      List.iter2 Var.set regs_v state;
      Inc.stabilize ();
      let nxt_state = List.map Inc.Observer.value_exn regs_o in
      (* XXX reapply register values, then recompute outputs *)
      List.iter2 Var.set regs_v nxt_state;
      Inc.stabilize ();
      let nxt_outputs = O.map Inc.Observer.value_exn outputs_o in
      nxt_state, nxt_outputs
    in

    reset, cycle

  let make f = 
    let i = I.map (fun (n,b) -> Signal.Comb.input n b) I.t in
    let o = f i in
    let reset, cycle = build_sim i o in
    reset, cycle

end


