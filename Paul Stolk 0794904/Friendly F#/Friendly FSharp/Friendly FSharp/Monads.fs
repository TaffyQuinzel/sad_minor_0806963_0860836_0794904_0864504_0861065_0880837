module Monads
  type Option<'a> = 
    | Just of 'a
    | None

  type OptionBuilder() =
    member this.Return (x :'a) = Just x
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

  // M<'a> = List<'a>

  // return : 'a -> M<'a>
  let ret (x:'a) : List<'a> = [x]

  // >>= : M<'a> -> ('a -> M<'b>) -> M<'b>
  let rec (>>=) (p:List<'a>) (k:'a -> List<'b>) : List<'b> =
    match p with
    | [] -> []
    | x::xs -> (k x) @ (xs >>= k) // O(N^2)

  type ListBuilder() =
    member this.Return x = ret x
    member this.Bind(p,k) = p >>= k
    member this.Zero() = []
  let lst = ListBuilder()
  
  let cartesian_product l1 l2 =
//    [
//      for x in l1 do
//      for y in l2 do
//      if x > y then
//        yield x,y
//    ]
    lst{
      let! x = l1
      let! y = l2
      if x > y then
        return x,y
    }

  // M<'a> = State<'s,'a>
  type State<'a,'s> = 's -> 'a * 's

  //wat er gebeurt is dat state bevat een functie en een geheugen status. Bij de return waarde wil je de nieuwe geheugen status
  // terug krijgen. die veroorzaakt is door het uitvoeren van de binnen gekregen fun
  let stateReturn (x:'a): State<'a,'s> = fun s -> x, s 

  type stateBuilder() =
   member this.Return = stateReturn

   // >>= : M<'a> -> State('a -> M<'b>) -> State<'a,'b>
  let stateBind (p : State<'a,'s>) (k: 'a-> State<'b, 's >) :State<'b, 's> =
    fun s-> 
      let (a,s0) = p s
      let (b,s1) = k a s0
      (b,s1)
    


