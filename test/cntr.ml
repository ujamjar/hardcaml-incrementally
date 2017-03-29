module I = struct
  type 'a t = {
    clk : 'a;
    clr : 'a;
    ena : 'a;
  }[@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    cnt : 'a[@bits 8];
  }[@@deriving hardcaml]
end

open HardCaml
open Signal.Comb
open I
open O

let f i = 
  let module Seq = (val (Signal.seq_sync ~clk:i.clk ~clr:i.clr) : Signal.Seq) in
  let cnt = Seq.reg_fb ~e:i.ena ~w:8 (fun d -> d +:. 1) in
  { cnt }

module B = HardCaml.Bits.Comb.IntbitsList
module G = Sim.Make(B)(I)(O)()

let test () = 
  let reset, cycle = G.make f in

  let sim = reset () in
  let ena () = { clk = B.constb "0"; clr = B.constb "0"; ena = B.constb "1" } in
  let clr () = { (ena ()) with clr = B.constb "1" } in
  let hlt () = { (ena ()) with ena = B.constb "0" } in

  let show_stats = G.Stats.print_diff stdout in
  let cycle sim d = 
    let sim,_,o = cycle sim d in
    Printf.printf "%i\n" (B.to_int o.cnt);
    show_stats ();
    sim
  in

  let sim = cycle sim (clr()) in
  let sim = cycle sim (ena()) in
  let sim = cycle sim (ena()) in
  let sim = cycle sim (ena()) in
  let sim = cycle sim (clr()) in
  let sim = cycle sim (ena()) in
  let sim = cycle sim (hlt()) in
  let sim = cycle sim (hlt()) in
  let sim = cycle sim (hlt()) in
  let _   = cycle sim (ena()) in
  ()

let () = test ()

