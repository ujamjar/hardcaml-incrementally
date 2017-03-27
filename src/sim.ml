(*
  Perform cycle accurate simulation using Incremental as the scheduling engine.

  TODO;

   * Implement the mux and concat nodes (how to go from ['a Inc.t list] to ['a list Inc.t]
   * Implement the clear/enable logic around registers
   * How do we deal with memories, in particular the read_address?

*)
open HardCaml

module Inc = Incremental_lib.Incremental.Make ()
module Var = Inc.Var

module Make(B : Comb.S)(I : Interface.S)(O : Interface.S) : sig

  type simulator
  type reset = unit -> simulator
  type cycle = simulator -> B.t I.t -> simulator * B.t O.t

  val make : (Signal.Comb.t I.t -> Signal.Comb.t O.t) -> reset * cycle

end = struct

  open Circuit
  open Signal.Types
  
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

  let build_sim inputs outputs = 

    (* set up initial incremental nodes for inputs, constants and registers *)
    let regs, mems, consts, _, remaining = find_elements (O.to_list outputs) in

    let inputs_v = I.(map (fun s -> Var.create @@ B.zero (width s)) inputs) in
    let inputs_w = I.(map Var.watch inputs_v) in

    let regs_v = List.map (fun s -> Var.create @@ B.zero (width s)) regs in
    let regs_w = List.map Var.watch regs_v in

    let imap = UidMap.empty in
    let imap = List.fold_left2 (fun m r i -> UidMap.add (uid r) i m) imap regs regs_w in
    (*let imap = List.fold_left add_mem map mems in*)
    let imap = List.fold_left 
        (fun m c -> UidMap.add (uid c) (Inc.const @@ B.constb @@ const_value c) m) 
        imap consts 
    in
    let imap = List.fold_left 
        (fun m (uid,v) -> UidMap.add uid v m)
        imap (I.to_list @@ I.map2 (fun a b -> (uid a),b) inputs inputs_w) 
    in

    (* construct incremental graph *)
    let rec create map signal = 
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

    let imap = List.fold_left (fun map s -> fst @@ create map s) imap (O.to_list outputs) in

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
    let regs_o = List.map (fun s -> Inc.observe @@ UidMap.find (uid s) imap) regs in

    let cycle state inps = 
      (* apply inputs *)
      ignore @@ I.(map2 Var.set inputs_v inps);
      (* apply current register values *)
      List.iter2 Var.set regs_v state;
      Inc.stabilize ();
      let nxt_state = List.map Inc.Observer.value_exn regs_o in
      (* XXX reapply register values, then recompute outputs *)
      (* List.iter2 Var.set regs_v nxt_state;
      Inc.stabilize (); *)
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


