module Make(B : HardCaml.Comb.S)(I : HardCaml.Interface.S)(O : HardCaml.Interface.S)() = struct

  module Inc = Sim.Make(B)(I)(O)()

  let make f = 
    let sim = Inc.make_inc f in
    let st = ref (Inc.reset sim ()) in

    let rf (_,b) = ref (B.zero b) in
    let i = I.(map rf t) in
    let o = O.(map rf t) in
    let n = O.(map rf t) in

    let port (n,_) p = n, p in
    let sim_in_ports = I.(to_list @@ map2 port t i) in
    let sim_out_ports = O.(to_list @@ map2 port t o) in
    let sim_out_ports_next = O.(to_list @@ map2 port t n) in

    let sim_reset () = st := Inc.reset sim () in
    let sim_cycle_comb0 () = 
      let x = Inc.cycle_comb0 sim !st I.(map (!) i) in
      ignore @@ O.map2 (:=) o x
    in
    let sim_cycle_seq () = st := Inc.cycle_seq sim in
    let sim_cycle_comb1 () = 
      let x = Inc.cycle_comb1 sim in
      ignore @@ O.map2 (:=) n x
    in
    let fail _ = failwith "incremental cyclesim: function not implemented" in
    let task () = () in

    {
      HardCaml.Cyclesim.Api.sim_in_ports;
      sim_out_ports;
      sim_out_ports_next;
      sim_internal_ports = [];
      sim_reset;
      sim_cycle_check = task;
      sim_cycle_comb0;
      sim_cycle_seq;
      sim_cycle_comb1;
      sim_lookup_signal = fail;
      sim_lookup_reg = fail;
      sim_lookup_memory = fail;
    }

end

