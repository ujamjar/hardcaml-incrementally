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
  I.{ O.c = i.a &: i.b;
      d = i.a |: i.b }

module B = HardCaml.Bits.Comb.IntbitsList
module G = Sim.Make(B)(I)(O)

let test () = 
  let reset, cycle = G.make f in
  
  let open I in
  let sim = reset () in
  let sim,o1 = cycle sim { a = B.vdd; b = B.gnd } in
  let sim,o2 = cycle sim { a = B.vdd; b = B.vdd } in

  let open O in
  Printf.printf "%i %i\n" (B.to_int o1.c) (B.to_int o1.d);
  Printf.printf "%i %i\n" (B.to_int o2.c) (B.to_int o2.d);

  ()

let () = test ()


