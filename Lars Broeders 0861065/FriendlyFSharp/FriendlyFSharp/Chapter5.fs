module Chapter5
  module Coroutines =
    open Microsoft.FSharp
    open Microsoft.FSharp.Core
    open System

    type Coroutine<'a> = Unit -> CoroutineStep<'a>
    and CoroutineStep<'a> =
      | Return of 'a
      | Yield of Coroutine<'a>
      | ArrowYield of Coroutine<'a>

    type CoroutineBuilder() =
      member this.Return(x: 'a): Coroutine<'a> =
        fun () -> Return x
      member this.Bind(p: Coroutine<'a>, k: 'a -> Coroutine<'b>): Coroutine<'b> =
        fun () ->
          match p () with
          | Return x -> k x ()
          | Yield p' -> Yield(this.Bind(p', k))
          | ArrowYield p' -> ArrowYield(this.Bind(p', k))
      member this.Combine(p1: Coroutine<'a>, p2: Coroutine<'b>): Coroutine<'b> =
        this.Bind(p1, fun _ -> p2)
      member this.Zero(): Coroutine<Unit> = this.Return()
      member this.ReturnFrom(s: Coroutine<'a>) = s
      member this.Delay s = s()
      member this.Run s = s

    let co = CoroutineBuilder()

    let yield_: Coroutine<Unit> =
      fun s -> Yield(fun s -> Return())
    let arrow_yield_: Coroutine<Unit> =
      fun s -> ArrowYield(fun s -> Return())
    let ignore_ (s: Coroutine<'a>): Coroutine<Unit> =
      co{
        let! _ = s
        return ()
      }

    let rec (.||) (s1: Coroutine<'a>) (s2: Coroutine<'b>): Coroutine<Choice<'a, 'b>> =
      fun s ->
        match s1 s, s2 s with
        | Return x, _ -> Return(Choice1Of2 x)
        | _, Return x -> Return(Choice2Of2 x)
        | ArrowYield k1, _ -> 
          co{
            let! res = k1
            return Choice1Of2 res
          } |> Yield
        | _, ArrowYield k2 ->
          co{
            let! res = k2
            return Choice2Of2 res
          } |> Yield
        | Yield k1, Yield k2 -> (.||) k1 k2 |> Yield

    let (.||>) s1 s2 = ignore_ (s1 .|| s2)

    let rec (=>) (c: Coroutine<bool>) (s: Coroutine<'a>): Coroutine<'a> =
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

    let rec repeat_ (s: Coroutine<Unit>): Coroutine<Unit> =
      co{
        do! s
        return! repeat_ s
      }

    let wait_doing (action: float -> Coroutine<Unit>) (interval: float): Coroutine<Unit> =
      let time: Coroutine<DateTime> =
        fun _ -> Return(DateTime.Now)
      co{
        let! t0 = time
        let rec wait() =
          co{
            let! t = time
            let dt = (t - t0).TotalSeconds
            if dt < interval then
              do! yield_
              do! action dt
              return! wait()
          }
        do! wait()
      }

    let wait = wait_doing (fun (dt:float) -> co{ return () })

  module PoliceChase =
    open System
    open System.Threading
    open Chapter2.Math
    open Coroutines

    [<Measure>]
    type Life

    type Ship = 
      {
        mutable Position: Vector2<m>
        mutable Velocity: Vector2<m/s>
        DryMass: float<kg>
        mutable Fuel: float<kg>
        MaxFuel: float<kg>
        Thrust: float<N/s>
        FuelBurn: float<kg/s>
        mutable Force: Vector2<N>
        mutable Integrity: float<Life>
        MaxIntegrity: float<Life>
        Damage: float<Life/s>
        WeaponsRange: float<m>
        mutable AI: Coroutine<Unit>
      }

      member this.Mass = this.DryMass + this.Fuel

    type Station =
      {
        Position: Vector2<m>
      }

    type PoliceChase =
      {
        PoliceStation: Station
        Patrol: Ship
        Pirate: Ship
        Cargo: Ship
      }

    let dt = 180.0<s>
    let field_size = 3.8e7<m>

    let impulse (self: Ship) (dir: Vector2<1>) (engine_power: float) =
      if self.Fuel > self.FuelBurn * engine_power * dt then
        do self.Force <- self.Thrust * dir * engine_power * dt
        do self.Fuel <- self.Fuel - self.FuelBurn * engine_power * dt
    
    let attack (self: Ship) (target: Ship) =
      co{
        do! yield_
        let dir = Vector2<_>.Normalize(target.Position - self.Position)
        let dist = (target.Position - self.Position).Length
        if dist > self.WeaponsRange * 0.8 then
          if self.Velocity.Length > 0.01<_> then
            let v_norm = self.Velocity.Normalized
            let dot = Vector2.Dot(dir, v_norm)
            if dot <= 0.0 then
              do impulse self (-self.Velocity.Normalized) 1.0
            elif dot < 0.5 then
              do impulse self (Vector2<_>.Normalize(-(self.Velocity.Normalized - dir * dot))) 0.3
            else
              do impulse self dir 0.1
            do! wait 1.0
          else
            do impulse self dir 1.0
            do! wait 1.0
        return ()
      }

    let reach_station (self: Ship) (s: PoliceChase) =
      co{
        do! yield_
        let dir = Vector2<_>.Normalize(s.PoliceStation.Position - self.Position)
        if Vector2<_>.Distance(s.PoliceStation.Position, self.Position) <= field_size *1.0e-1 then
          let zero_velocity =
            co{
              do! yield_
              return self.Velocity <- Vector2<_>.Zero
            }
          do! wait_doing (fun _ -> zero_velocity) 5.0
          do self.Integrity <- self.MaxIntegrity
          do self.Fuel <- self.MaxFuel
        elif self.Velocity.Length > 0.01<_> then
          let dot = Vector2<1>.Dot(self.Velocity.Normalized, dir)
          if dot <= 0.0 then 
            do impulse self (-self.Velocity.Normalized) 1.0
          elif dot < 0.5 then
            do impulse self (Vector2<_>.Normalize(-(self.Velocity.Normalized - dir * dot))) 0.3
          else
            do impulse self dir 0.2
          do! wait 1.0
        else
          do impulse self dir 1.0
          do! wait 1.0
        return ()
      }

    let patrol_ai (s: PoliceChase) =
      let self = s.Patrol
      let healthy_and_fueled =
        co{
          do! yield_
          return self.Integrity > self.MaxIntegrity * 0.4 && self.Fuel > self.MaxFuel * 0.4
        }
      let need_docking =
        co{
          do! yield_
          let! h = healthy_and_fueled
          return not h
        }
      repeat_ ((healthy_and_fueled => attack self (s.Pirate)) .||> (need_docking => reach_station self s))

    let pirate_ai (s: PoliceChase) =
      let self = s.Pirate
      let patrol_near =
        co{
          do! yield_
          return Vector2<_>.Distance(self.Position, s.Patrol.Position) < s.Patrol.WeaponsRange
        }
      let patrol_far =
        co{
          let! n = patrol_near
          return not n
        }
      repeat_ ((patrol_near => (attack (s.Pirate) (s.Patrol))) .||> (patrol_far => (attack (s.Pirate) (s.Cargo))))

    let cargo_ai (s: PoliceChase) =
      let self = s.Cargo
      co{
        do! yield_
        do! reach_station self s
      } |> repeat_

    let s0() =
      let s =
        {
          PoliceStation = { Position = { X = field_size; Y = field_size } * 0.25 }
          Patrol =
            {
              Position = { X = field_size; Y = field_size } * 0.25
              Velocity =  Vector2<_>.Zero
              DryMass = 4.5e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(50.0 * 180.0)
              Thrust = 5.0e6<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 100.0<_>
              MaxIntegrity = 100.0<_>
              Damage = 1.0e-1<_>/180.0
              WeaponsRange = field_size*0.1
              AI = co{ return () }
            }
          Pirate =
            {
              Position = { X = field_size; Y = field_size }*0.75
              Velocity = Vector2<_>.Zero
              DryMass = 3.0e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(30.0*180.0)
              Thrust = 5.0e5<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 75.0<_>
              MaxIntegrity = 75.0<_>
              Damage = 2.0e-1<_>/180.0
              WeaponsRange = field_size*0.15
              AI = co{ return () }
            }
          Cargo =
            {
              Position = { X = field_size; Y = field_size*0.7 }*0.7
              Velocity = Vector2<_>.Zero
              DryMass = 2.3e6<_>
              Fuel = 3.5e8<_>*0.3
              MaxFuel = 3.5e8<_>
              FuelBurn = 3.5e6<_>/180.0
              Thrust = 3.4e6<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 300.0<_>
              MaxIntegrity = 300.0<_>
              Damage = 1.0e-3<_>/180.0
              WeaponsRange = field_size*0.1
              AI = co{ return () }
            }
        }
      do s.Patrol.AI <- patrol_ai s
      do s.Pirate.AI <- pirate_ai s
      do s.Cargo.AI  <- cargo_ai s
      s

    let co_step =
      function
      | Return x -> co{ return x }
      | Yield k -> k
      | ArrowYield k -> k

    let ship_step (s: Ship) =
      do s.Position <- s.Position + s.Velocity * dt
      do s.Velocity <- s.Velocity + dt * s.Force / s.Mass
      do s.Force <- Vector2<_>.Zero
      do s.AI <- co_step (s.AI())

    let simulation_step (s: PoliceChase) =
      do ship_step s.Patrol
      do ship_step s.Pirate
      do ship_step s.Cargo
      if Vector2<_>.Distance(s.Patrol.Position, s.Pirate.Position) < s.Patrol.WeaponsRange then
        do s.Pirate.Integrity <- s.Pirate.Integrity - s.Patrol.Damage * dt
      if Vector2<_>.Distance(s.Cargo.Position, s.Pirate.Position) < s.Cargo.WeaponsRange then
        do s.Pirate.Integrity <- s.Pirate.Integrity - s.Cargo.Damage * dt
      if Vector2<_>.Distance(s.Patrol.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
        do s.Patrol.Integrity <- s.Patrol.Integrity - s.Pirate.Damage * dt
      elif Vector2<_>.Distance(s.Cargo.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
        do s.Cargo.Integrity <- s.Cargo.Integrity - s.Pirate.Damage * dt

    let print(s: PoliceChase) =
      do Console.Clear()
      let set_cursor (v: Vector2<_>) =
        Console.SetCursorPosition(
          (((v.X / field_size) * 79.0) |> int) - 1 |> max 0 |> min 79,
          ((v.Y / field_size) * 23.0) |> int |> max 0 |> min 23
        )
      let set_cursor_on_ship (s: Ship) = set_cursor (s.Position)
      let set_cursor_on_station (s: Station) = set_cursor (s.Position)
      do set_cursor_on_station s.PoliceStation
      do Console.Write("S")
      do set_cursor_on_ship (s.Patrol)
      let ship_fuel (s:Ship) = (9.0*s.Fuel/s.MaxFuel).ToString("#.")
      let ship_integrity (s:Ship) = (9.0*s.Integrity/s.MaxIntegrity).ToString("#.")
      do Console.Write((ship_fuel s.Patrol) + "P" + (ship_integrity s.Patrol))
      do set_cursor_on_ship (s.Pirate)
      do Console.Write((ship_fuel s.Pirate) + "X" + (ship_integrity s.Pirate))
      do set_cursor_on_ship (s.Cargo)
      do Console.Write((ship_fuel s.Cargo) + "C" + (ship_integrity s.Cargo))
      do Console.SetCursorPosition(0, 0)
      do Thread.Sleep(10)

    let simulation() =
      let s = s0()
      let rec simulation() =
        do print s
        if s.Patrol.Integrity > 0.0<_> && s.Pirate.Integrity > 0.0<_> && s.Cargo.Integrity > 0.0<_> then
          do simulation (simulation_step s)
      do simulation()