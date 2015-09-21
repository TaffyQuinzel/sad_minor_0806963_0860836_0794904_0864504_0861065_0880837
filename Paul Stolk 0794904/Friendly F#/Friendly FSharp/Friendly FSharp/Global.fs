namespace Global

 module Math =

    [<Measure>]
    type meters

    [<Measure>]
    type kg

    [<Measure>]
    type seconds

    [<Measure>]
    type Newton = kg * meters / seconds^2

 module MathVector2 =
    type Vector2<[<Measure>] 'a> =
      {
        X: float<'a>
        Y: float<'a>
      }

      static member Zero: Vector2<'a> =
        { X = 0.0<_>; Y = 0.0<_> }
      static member (+) (v1: Vector2<'a>, v2: Vector2<'a>): Vector2<'a> =
        { X = v1.X + v2.X; Y = v1.Y + v2.Y }
      static member (+) (v: Vector2<'a>, k: float<'a>): Vector2<'a> =
        { X = v.X + k; Y = v.Y + k }
      static member (+) (k:float<'a>, v: Vector2<'a>): Vector2<'a> =
        v + k
      static member (~-) (v: Vector2<'a>): Vector2<'a> =
        { X = -v.X; Y = -v.Y }
      static member (-) (v1: Vector2<'a>, v2: Vector2<'a>): Vector2<'a> =
        v1 + (-v2)
      static member (-) (v: Vector2<'a>, k: float<'a>): Vector2<'a> =
        v + (-k)
      static member (-) (k: float<'a>, v: Vector2<'a>): Vector2<'a> =
        k + (-v)
      static member (*) (v1: Vector2<'a>, v2: Vector2<'b>): Vector2<'a * 'b> =
        { X = v1.X * v2.X; Y = v1.Y * v2.Y }
      static member (*) (v: Vector2<'a> , f: float<'b>): Vector2<'a * 'b> =
        { X = v.X * f; Y = v.Y * f }
      static member (*) (f: float<'b>, v: Vector2<'a>): Vector2<'b * 'a> =
        { X = f * v.X; Y = f * v.Y }
      static member (/) (v: Vector2<'a>, f: float<'b>): Vector2<'a / 'b> =
        v * (1.0 / f)
      member this.Length: float<'a> =
        sqrt((this.X * this.X + this.Y * this.Y))
      member this.Normalized = 
        this / this.Length
      static member Distance (v1: Vector2<'a>, v2: Vector2<'a>): float<'a> =
        (v1 - v2).Length
      static member Normalize(v: Vector2<'a>): Vector2<1> =
        v.Normalized
      static member Dot (v1: Vector2<'a>, v2: Vector2<'b>): float<'a * 'b> =
        v1.X * v2.X + v1.Y * v2.Y

 module MathVector3 =
    type Vector3<[<Measure>] 'a> =
      {
        X: float<'a>
        Y: float<'a>
        Z: float<'a>
      }

      static member Zero: Vector3<'a> =
        { X = 0.0<_>; Y = 0.0<_>; Z = 0.0<_> }
      static member (+) (v1: Vector3<'a>, v2: Vector3<'a>): Vector3<'a> =
        { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
      static member (+) (v: Vector3<'a>, k: float<'a>): Vector3<'a> =
        { X = v.X + k; Y = v.Y + k; Z = v.Z + k }
      static member (+) (k:float<'a>, v: Vector3<'a>): Vector3<'a> =
        v + k
      static member (~-) (v: Vector3<'a>): Vector3<'a> =
        { X = -v.X; Y = -v.Y; Z = -v.Z }
      static member (-) (v1: Vector3<'a>, v2: Vector3<'a>): Vector3<'a> =
        v1 + (-v2)
      static member (-) (v: Vector3<'a>, k: float<'a>): Vector3<'a> =
        v + (-k)
      static member (-) (k: float<'a>, v: Vector3<'a>): Vector3<'a> =
        k + (-v)
      static member (*) (v1: Vector3<'a>, v2: Vector3<'b>): Vector3<'a * 'b> =
        { X = v1.X * v2.X; Y = v1.Y * v2.Y; Z = v1.Z * v2.Z }
      static member (*) (v: Vector3<'a> , f: float<'b>): Vector3<'a * 'b> =
        { X = v.X * f; Y = v.Y * f; Z = v.Z * f }
      static member (*) (f: float<'b>, v: Vector3<'a>): Vector3<'b * 'a> =
        { X = f * v.X; Y = f * v.Y; Z = f * v.Z }
      static member (/) (v: Vector3<'a>, f: float<'b>): Vector3<'a / 'b> =
        v * (1.0 / f)
      member this.Length: float<'a> =
        sqrt((this.X * this.X + this.Y * this.Y + this.Z * this.Z))
      member this.Normalized = 
        this / this.Length
      static member Distance (v1: Vector3<'a>, v2: Vector3<'a>): float<'a> =
        (v1 - v2).Length
      static member Normalize(v: Vector3<'a>): Vector3<1> =
        v.Normalized
      static member Dot (v1: Vector3<'a>, v2: Vector3<'b>): float<'a * 'b> =
        v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

 module Coroutines =
    open Microsoft.FSharp
    open Microsoft.FSharp.Core
    open System

    type Coroutine<'a> = Unit -> CoroutineStep<'a>
    and CoroutineStep<'a> =
      Return of 'a
      | Yield of Coroutine<'a>
      | ArrowYield of Coroutine<'a>

    type CoroutineBuilder() =
      member this.Return(x:'a) : Coroutine<'a> =
        fun () -> Return x
      member this.Bind(p : Coroutine<'a>, k : 'a -> Coroutine<'b>) : Coroutine<'b> =
        fun () ->
          match p () with
          | Return x -> k x ()
          | Yield p' -> Yield(this.Bind(p',k))
          | ArrowYield p' -> ArrowYield(this.Bind(p',k))
      member this.Combine(p1:Coroutine<'a>, p2:Coroutine<'b>) : Coroutine<'b> =
        this.Bind(p1,fun _ -> p2)
      member this.Zero() : Coroutine<Unit> = this.Return()
      member this.ReturnFrom(s:Coroutine<'a>) = s
      member this.Delay s = s()
      member this.Run s = s

    let co = CoroutineBuilder()
    let yield_ : Coroutine<Unit> =
      fun s -> Yield(fun s -> Return())
    let arrow_yield_ : Coroutine<Unit> =
      fun s -> ArrowYield(fun s -> Return())
    let ignore_ (s:Coroutine<'a>) : Coroutine<Unit> =
      co{
        let! _ = s
        return ()
      }
    let rec (.||) (s1:Coroutine<'a>) (s2:Coroutine<'b>) :
      Coroutine<Choice<'a,'b>> =
      fun s ->
        match s1 s,s2 s with
        | Return x,_ -> Return(Choice1Of2 x)
        | _,Return y -> Return(Choice2Of2 y)
        | ArrowYield k1,_ ->
          co{
            let! res = k1
            return Choice1Of2 res
          } |> Yield
        | _,ArrowYield k2 ->
          co{
            let! res = k2
            return Choice2Of2 res
          } |> Yield
        | Yield k1,Yield k2 -> (.||) k1 k2 |> Yield
    let (.||>) s1 s2 = ignore_ (s1 .|| s2)
    let rec (=>) (c:Coroutine<bool>) (s:Coroutine<'a>) :
      Coroutine<'a> =
      co{
        let! x = c
        if x then
          do! arrow_yield_
          let! res = s
          return res
        else
          do! yield_
          return! (=>) c s
        }
    let rec repeat_ (s:Coroutine<Unit>) : Coroutine<Unit> =
      co{
        do! s
        return! repeat_ s
      }
    let wait_doing (action:float -> Coroutine<Unit>)
      (interval:float) : Coroutine<Unit> =
      let time : Coroutine<DateTime> =
        fun _ -> Return(DateTime.Now)
      co{
        let! t0 = time
        let rec wait() =
          co{
            let! t = time
            let dt = (t-t0).TotalSeconds
            if dt < interval then
              do! yield_
              do! action dt
              return! wait()
          }
        do! wait()
      }
    let wait = wait_doing (fun (dt:float) -> co{ return () })
