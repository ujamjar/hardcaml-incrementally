(** 
 
    {2 HardCaml LWT testbench framework. }

  Testbenches in HardCaml are synchronised to a [cycle] function
  which updates the simulation state.  The hardware under test may
  have 1 or more relatively distinct input/output interfaces (for
  example multiple input/output fifos, register interfaces, ddr
  interfaces etc).

  Tranditionally, if more than 1 interface needed modelling in the
  testbench, then each process would have to be converted into a
  statemachine that was updated as part of the simulation cycle.

  This code allows for multiple processes to be run in parallel
  while still synchronising around the clock.

    {2 Programming model}

  A testbench consists of a number of tasks of type [t -> t Lwt.t].
  The type [t] holds the simulation state and allows each Lwt 
  thread to be synchronised with the simulation cycle.  [t] must
  be passed through each API function (it is purely functional
  and gets updated by successive API functions).
  
  The functions [cycle1] and [spawn] provide the core API.
  [cycle1] synchronises all tasks together, collects circuit
  inputs, runs the simulation cycle and distributes circuit outputs.
  [spawn] allows nww tasks to start running (within the current
  simulation cycle).  Tasks are arranged in a tree like fashion.
  The inital task is the root, and spawns within spawns are
  (nearer to) the leaves.

  The [set] function (along with [setsome] and [setall]) allow
  tasks to set circuit inputs.  Multiple tasks can set the same
  input and sets nearer leaves take priority.

  note; this is the current behaviour and may change or be
        extended ie to allow priority to be specified.
        The priority among tasks at the same level is the
        latest spawned task takes precedence.

*)

open HardCaml

module Make(B : Comb.S)(I : Interface.S)(O : Interface.S)() : sig

  type task_req  

  (** simulation testbench data type *)
  type t = private
    {
      (* mailbox variables used to synchronise to the clock cycle *)
      vreq : task_req Lwt_mvar.t;
      vresp : B.t O.t Lwt_mvar.t;
      (* child tasks *)
      children : t list;
      (* inputs *)
      inputs : B.t option I.t;
      (* cycle logging function *)
      log : log option;
    }

  and log = t -> unit Lwt.t

  (** type of simulation tasks synchronised to the clock *)
  type task = t -> t Lwt.t

  (* {2 cycles, task spawning and utility functions} *)

  (** cycle the clock, return circuit outputs *)
  val cycle1 : t -> (t * B.t O.t) Lwt.t

  (** cycle the clock n>=1 times *)
  val cycle : ?n:int -> t -> (t * B.t O.t) Lwt.t

  (** spawn a new simulation task synchronised to each cycle *)
  val spawn : ?log:log -> task -> t -> t Lwt.t

  (** [repeat n task sim] repeats the task n times *)
  val repeat : int -> task -> t -> t Lwt.t

  (** [delay n task sim] delay for n cycles then run task *)
  val delay : int -> task -> t -> t Lwt.t

  (** [Lwt.return] *)
  val return : 'a -> 'a Lwt.t

  (** perform a simulation cycle and return *)
  val return_cycle : t -> t Lwt.t

  (** {2 setting circuit inputs} *)

  (** input field accessors *)
  val i : bool I.t I.t

  (** input structure with all fields set to none *)
  val inone : B.t option I.t

  (** set an input field *)
  val set : bool I.t -> B.t -> t -> t Lwt.t

  (** set some inputs *)
  val setsome : B.t option I.t -> t -> t Lwt.t

  (** set all inputs *)
  val setall : B.t I.t -> t -> t Lwt.t

  (** {2 testbench simulation} *)

  (** incremental simulator *)
  module Sim : module type of Sim.Make(B)(I)(O)()

  (** run testbench *)
  val run : ?log:log -> (Sim.reset * Sim.cycle) -> task -> unit Lwt.t

end = struct

  open Lwt.Infix

  type task_req = 
    | Cycle of B.t option I.t 
    | Finish 

  type t = 
    {
      (* mailbox variables used to synchronise to the clock cycle *)
      vreq : task_req Lwt_mvar.t;
      vresp : B.t O.t Lwt_mvar.t;
      (* child tasks *)
      children : t list;
      (* inputs *)
      inputs : B.t option I.t;
      (* cycle logging function *)
      log : log option;
    }


  and log = t -> unit Lwt.t

  type task = t -> t Lwt.t

  let merge i1 i2 = I.map2 
      (fun i1 i2 ->
        match i1, i2 with
        | _, Some(d) -> Some(d)
        | Some(d), _ -> Some(d)
        | _ -> None) i1 i2

  let inone = I.map (fun _ -> None) I.t

  let dolog t children inputs = 
    match t.log with
    | None -> Lwt.return ()
    | Some(f) -> f { t with children; inputs }

  let cycle1 t = 
    (* wait for active children to notify *)
    let%lwt children = Lwt_list.filter_map_p 
      (fun t -> 
        Lwt_mvar.take t.vreq >>= function Cycle i -> Lwt.return_some (t,i)
                                        | Finish -> Lwt.return_none) 
      t.children 
    in
    let children = List.map fst children and inputs = List.map snd children in
    let inputs = List.fold_left merge t.inputs inputs in
    let%lwt () = dolog t children inputs in
    (* notify parent *)
    let%lwt () = Lwt_mvar.put t.vreq (Cycle inputs) in
    (* wait for state from parent *)
    let%lwt o = Lwt_mvar.take t.vresp in
    (* broadcast to children *)
    let%lwt () = Lwt_list.iter_p (fun t -> Lwt_mvar.put t.vresp o) children in
    let t = { t with children; inputs=inone } in
    Lwt.return (t,o)

  let rec cycle ?(n=1) t = 
    if n < 1 then Lwt.fail_with "cycle must be for >= 1 cycle"
    else if n = 1 then
      cycle1 t
    else
      cycle1 t >>= fun (t,_) -> cycle ~n:(n-1) t

  let rec with_finish t = 
    if t.children = [] then 
      Lwt_mvar.put t.vreq Finish >> Lwt.return t
    else
      (* keep running while children are active *)
      let%lwt t,_ = cycle1 t in
      with_finish t

  let task' ?log () = 
    {
      vreq = Lwt_mvar.create_empty ();
      vresp = Lwt_mvar.create_empty ();
      children = [];
      inputs = inone;
      log;
    }

  let async task t = Lwt.async (fun () -> task t >>= with_finish)

  let spawn ?log k t = 
    (* generate communications variables *)
    let n = task' ?log () in
    (* run the thread *)
    let () = async k n in
    (* update children *)
    Lwt.return { t with children = n :: t.children }

  let rec repeat n f t = 
    if n <= 0 then Lwt.return t
    else
      let%lwt t = f t in
      repeat (n-1) f t

  let rec delay n f t = 
    if n <= 0 then f t
    else
      cycle t >>= fun (t,_) -> delay (n-1) f t

  let i = 
    I.map (fun (n,_) -> I.map (fun (m,_) -> n=m) I.t) I.t

  let set fld v t = 
    Lwt.return 
      { t with inputs = I.map2 (fun yn prv -> if yn then Some(v) else prv) fld t.inputs }

  let setsome v t = Lwt.return { t with inputs = merge t.inputs v }

  let setall v t = Lwt.return { t with inputs = I.map (fun x -> Some(x)) v }

  let return = Lwt.return

  let return_cycle sim = cycle1 sim >>= fun (sim,_) -> return sim

  module Sim = Sim.Make(B)(I)(O)()

  let run ?log (reset, cycle) task =
 
    let t = task' ?log () in
    let () = async task t in

    let rec loop prev sim = 
      match%lwt Lwt_mvar.take t.vreq with
      | Cycle inputs -> begin
        (* apply inputs *)
        let inputs = 
          I.map2 (fun p d -> match d with
                             | None -> p
                             | Some(d) -> d) prev inputs
        in
        (* simulation cycle *)
        let sim,_,o = cycle sim inputs in
        (* send back outputs *)
        let%lwt () = Lwt_mvar.put t.vresp o in
        (* loop *)
        loop inputs sim
      end
      | Finish -> Lwt.return ()
    in

    loop 
      I.(map (fun (_,b) -> B.zero b) t) 
      (reset ())

end

module Test = struct

  module I = struct
    type 'a t = {
      clk : 'a;
      clr : 'a;
      ena : 'a;
      d : 'a[@bits 8];
    }[@@deriving hardcaml,show]
  end

  module O = struct
    type 'a t = {
      cnt : 'a[@bits 8];
      q : 'a[@bits 8];
    }[@@deriving hardcaml]
  end

  open HardCaml
  open Signal.Comb
  open I
  open O

  let f i = 
    let module Seq = (val (Signal.seq_sync ~clk:i.clk ~clr:i.clr) : Signal.Seq) in
    let cnt = Seq.reg_fb ~e:i.ena ~w:8 (fun d -> d +:. 1) in
    { cnt; q = i.d }

  module B = HardCaml.Bits.Comb.IntbitsList
  module Tb = Make(B)(I)(O)()

  open Lwt.Infix

  let ppvec fmt v = Format.fprintf fmt "%s"
      (match v with None -> "?" | Some(v) -> string_of_int @@ B.to_int v)

  (* simple counter test *)
  let test_cntr sim = 
    let open Tb in
    let%lwt sim = set i.clr B.gnd sim in
    let%lwt sim = set i.ena B.vdd sim in
    let print sim = 
      let%lwt sim,o = cycle sim in
      let%lwt () = Lwt_io.printf "%i\n" (B.to_int o.cnt) in
      return sim
    in
    let%lwt sim = repeat 5 print sim in
    let%lwt sim = set i.clr B.vdd sim in
    let%lwt sim,_ = cycle sim in
    let%lwt sim = set i.clr B.gnd sim in
    let%lwt sim = repeat 5 print sim in
    return sim

  let tb1 () = 
    let sim = Tb.Sim.make f in
    Lwt_main.run @@ Tb.run sim test_cntr

  (* test thread spawning *)
  let test_spawner sim = 
    let open Tb in

    let%lwt sim = spawn ~log:(fun t -> Lwt_io.printf "====> 1\n")
      (fun sim ->
        let%lwt sim = setsome { inone with clr = Some(B.gnd); ena = Some(B.vdd) } sim in
        let%lwt sim = spawn 
          ~log:(fun t -> Lwt_io.printf "====> 4\n") 
          (fun sim -> repeat 11 return_cycle sim) sim 
        in
        repeat 7 return_cycle sim)
      sim
    in
    let%lwt sim = spawn ~log:(fun t -> Lwt_io.printf "====> 2\n")
      (fun sim -> repeat 3 return_cycle sim)
      sim
    in
    let%lwt sim = spawn ~log:(fun t -> Lwt_io.printf "====> 3\n")
      (fun sim -> repeat 9 return_cycle sim)
      sim
    in

    let%lwt sim = repeat 5
      (fun sim -> 
        cycle1 sim >>= fun (sim,o) -> 
        Lwt_io.printf "[%i]\n" (B.to_int o.cnt) >> 
        return sim) sim 
    in

    return sim

  let tb2 () = 
    let sim = Tb.Sim.make f in
    Lwt_main.run @@ Tb.run ~log:(fun t -> Lwt_io.printf "======> TOP\n") sim test_spawner

  (* test inputs *)

  let print_t id (sim : Tb.t) = 
    Lwt_io.printf "%s: %s\n" id (I.show ppvec sim.Tb.inputs)

  let test_inputs sim = 
    let open Tb in
    let%lwt sim = 
      spawn
        ~log:(print_t "[1]")
        (fun sim -> 
           return sim >>=
           set i.ena B.vdd >>=
           set i.clr B.vdd >>=
           set i.d B.(consti 8 10) >>=
           return_cycle >>=
           spawn 
             ~log:(print_t "[2]")
             (fun sim -> 
                return sim >>=
                set i.ena B.vdd >>=
                set i.d B.(consti 8 20) >>=
                return_cycle) >>= 
           return_cycle >>=
           set i.d B.(consti 8 30) >>=
           return_cycle 
        ) sim
    in
    return sim >>=
    set i.ena B.gnd >>=
    set i.d B.(consti 8 100) >>=
    return_cycle >>= 
    set i.ena B.gnd >>=
    set i.d B.(consti 8 110) >>=
    return_cycle 

  let tb3 () = 
    let sim = Tb.Sim.make f in
    Lwt_main.run @@ Tb.run ~log:(print_t "top") sim test_inputs
    
end


