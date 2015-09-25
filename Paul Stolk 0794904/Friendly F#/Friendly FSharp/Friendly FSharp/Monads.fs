module Monads
  type Option<'a> = 
    | Just of 'a
    | None

  type OptionBuilder() =
    member this.Return x :'a = Just x
    member this.Bind (o, f)  =
      match o with
      | Just x -> f x 
      | None -> None

  let opt = OptionBuilder()

  let i = Just 100
  let j = Just -150

  let sum = 
    opt
      {
        let! i_v = i
        let! j_v = j
        return j_v+i_v
      }