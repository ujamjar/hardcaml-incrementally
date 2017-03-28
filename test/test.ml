(* simple test of combinatorial logic *)

module I = struct
  type 'a t = {
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
  let reg = reg r_full vdd in
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
  let show o = Printf.printf "%i %i\n" (B.to_int o.c) (B.to_int o.d); in
  let sim,o = cycle sim { a = B.vdd; b = B.gnd } in
  show o;
  let sim,o = cycle sim { a = B.vdd; b = B.vdd } in
  show o;
  let sim,o = cycle sim { a = B.vdd; b = B.vdd } in
  show o;
  let sim,o = cycle sim { a = B.vdd; b = B.vdd } in
  show o;

  ()

let () = test ()


