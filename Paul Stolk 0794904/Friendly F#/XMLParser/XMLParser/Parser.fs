module XMLParser
(*
Wat is xml. XML heeft een openings tag '<' en een sluit tag '>' Daarnaast begint een object altijd met <naam> en sluit het object met </naam>
als dit dit er niet in staat moet de parser een error geven. 
Daarnaast kan een object ook nog een attribuut bevatten. 


*)
  type Result<'a> =
    | Success of 'a * list<char> 
    | Error
    
  type Parser<'a> = list<char> -> Result<'a>

  // Binding, voor de parser die een programma binnen krijgt en een functie. 
  let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun stream ->
        match p stream with
        | Success(x, rest) -> (f x) rest
        | Error -> Error
  
  let ret x : Parser<'a> = fun stream  -> Success(x, stream)

  type ParseBuilder() = 
    member this.Return(x) = ret x
    member this.Bind(p, f) = p >>= f
    member this.Zero = fun stream -> Error

  let parse = ParseBuilder()

