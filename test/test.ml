(* simple test of combinatorial logic *)

module I = struct
  type 'a t = {
    ena : 'a;
    a : 'a;
    b : 'a;
  }[@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    c : 'a;
    d : 'a;
  }[@@deriving hardcaml]
end

open HardCaml.Signal.Comb

let f i = 
  let open I in
  let open HardCaml.Signal.Seq in
  let reg = reg r_full i.ena in
  let c = i.a &: i.b in
  let d = i.a |: i.b in
  let c = reg c in
  { O.c; d }

module B = HardCaml.Bits.Comb.IntbitsList
module G = Sim.Make(B)(I)(O)()

let test () = 
  let reset, cycle = G.make f in
  
  let open I in
  let open O in
  let sim = reset () in
  let show o = Printf.printf "%i %i " (B.to_int o.c) (B.to_int o.d); in
  let show_stats = G.Stats.print_diff stdout in
  let show o n = 
    show o; show n; Printf.printf "\n";
    show_stats ()
  in
  let def = { ena = B.vdd; a = B.gnd; b = B.gnd } in
  let sim,o,n = cycle sim { def with a = B.vdd; b = B.gnd } in
  show o n;
  let sim,o,n = cycle sim { def with a = B.vdd; b = B.vdd } in
  show o n;
  let sim,o,n = cycle sim { def with a = B.vdd; b = B.vdd } in
  show o n;
  let sim,o,n = cycle sim { def with a = B.vdd; b = B.vdd } in
  show o n;

  ()

let () = test ()


