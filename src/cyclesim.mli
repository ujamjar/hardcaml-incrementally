module Make(B : HardCaml.Comb.S)(I : HardCaml.Interface.S)(O : HardCaml.Interface.S)() : sig

  val make : 
    (HardCaml.Signal.Comb.t I.t -> HardCaml.Signal.Comb.t O.t) -> 
    B.t HardCaml.Cyclesim.Api.cyclesim

end
