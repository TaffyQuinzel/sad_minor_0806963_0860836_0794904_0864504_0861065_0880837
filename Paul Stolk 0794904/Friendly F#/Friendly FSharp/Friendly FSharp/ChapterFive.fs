namespace ChapterFive

  module PoliceChase =
    open System
    open System.Threading
    open Global.Math
    open Global.MathVector2
    open Global.Coroutines

    [<Measure>]
    type Life

    type Ship =
      {
        mutable Position : Vector2<meters>
        mutable Velocity : Vector2<meters/seconds>
        DryMass : float<kg>
        mutable Fuel : float<kg>
        MaxFuel : float<kg>
        Thrust : float<Newton/seconds>
        FuelBurn : float<kg/seconds>
        mutable Force : Vector2<Newton>
        mutable Integrity : float<Life>
        MaxIntegrity : float<Life>
        Damage : float<Life/seconds>
        WeaponsRange : float<meters>
        mutable AI : Coroutine<Unit>
      }
      member this.Mass = this.DryMass+this.Fuel
    
    type Station =
      {
        Position : Vector2<meters>
      }

    type PoliceChase =
      {
        PoliceStation : Station
        HeadPatrol : Ship
        subPatrol1 : Ship
        subPatrol2 : Ship
        HeadPirate : Ship
        Pirate : Ship
        Cargo : Ship
      }
      member PoliceChase.PoliceShips = seq {yield PoliceChase.HeadPatrol; yield PoliceChase.subPatrol1; yield PoliceChase.subPatrol2;}
      member PoliceChase.PirateShips = seq {yield PoliceChase.HeadPirate; yield PoliceChase.Pirate; }

    let dt = 180.0<seconds>
    let field_size = 3.8e7<meters>
    let impulse (self:Ship)
      (dir:Vector2<1>) (engine_power:float) =
      if self.Fuel > self.FuelBurn*engine_power*dt then
        do self.Force <- self.Thrust*dir*engine_power*dt
        do self.Fuel <- self.Fuel-self.FuelBurn* engine_power*dt

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

    let reach_station (self:Ship) (s:PoliceChase) =
      co{
        do! yield_
        let dir =
          Vector2<_>.Normalize(s.PoliceStation.Position -
            self.Position)
        if Vector2<_>.Distance(s.PoliceStation.Position,self.Position) <= field_size*1.0e-1 then
          let zero_velocity =
            co{
              do! yield_
              return self.Velocity <- Vector2<_>.Zero
            }
          do! wait_doing (fun _ -> zero_velocity) 5.0
          do self.Integrity <- self.MaxIntegrity
          do self.Fuel <- self.MaxFuel
        elif self.Velocity.Length > 0.01<_> then
          let dot =
            Vector2<1>.Dot(self.Velocity.Normalized,dir)
          if dot <= 0.0 then
            do impulse self (-self.Velocity.Normalized) 1.0
          elif dot < 0.5 then
            do do impulse self (Vector2<_>.Normalize(-(self.Velocity.Normalized - dir * dot))) 0.3
          else
            do impulse self dir 0.2
          do! wait 1.0
        else
          do impulse self dir 1.0
          do! wait 1.0
        return ()
      }
    
    let patrol_ai (self:Ship) (s:PoliceChase) =
      let closestPirate() : Ship =
        s.PirateShips |> Seq.minBy (fun ship ->  Vector2<_>.Distance(self.Position, ship.Position))     
      let healthy_and_fueled =
        co{
          do! yield_
          return self.Integrity > self.MaxIntegrity*0.4 && self.Fuel > self.MaxFuel*0.4
        }
      let need_docking =
        co{
          do! yield_
          let! h = healthy_and_fueled
          return not h
        }
      repeat_
        ((healthy_and_fueled => attack self (closestPirate())) .||>
          (need_docking => reach_station self s))

    let pirate_ai (self:Ship) (s: PoliceChase) =
      let closestShip() : Ship =
        s.PoliceShips |> Seq.minBy (fun ship ->  Vector2<_>.Distance(self.Position, ship.Position))     
      let patrol_near =
        co{
          do! yield_
          return Vector2<_>.Distance(self.Position, closestShip().Position) < s.subPatrol1.WeaponsRange
        }
      let patrol_far =
        co{
          let! n = patrol_near
          return not n
        }
      repeat_ ((patrol_near => (attack (s.Pirate) (closestShip()))) .||> (patrol_far => (attack (s.Pirate) (s.Cargo))))
    
    let cargo_ai (s: PoliceChase) =
      let self = s.Cargo
      co{
        do! yield_
        do! reach_station self s
      } |> repeat_

    let s0() =
      let s =
        {
          PoliceStation =
            { 
              Position = { X = field_size; Y = field_size }*0.25 
            }
          HeadPatrol =
            {
              Position = { X = field_size; Y = field_size }*0.78
              Velocity = Vector2<_>.Zero
              DryMass = 4.5e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(50.0*180.0)
              Thrust = 5.0e6<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 200.0<_>
              MaxIntegrity = 200.0<_>
              Damage = 2.0e-1<_>/180.0
              WeaponsRange = field_size*0.1
              AI = co{ return () }
            }
          subPatrol1 =
            {
              Position = { X = field_size; Y = field_size }*0.35
              Velocity = Vector2<_>.Zero
              DryMass = 4.5e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(50.0*180.0)
              Thrust = 5.0e6<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 50.0<_>
              MaxIntegrity = 50.0<_>
              Damage = 1.0e-1<_>/180.0
              WeaponsRange = field_size*0.1
              AI = co{ return () }
            }
          subPatrol2 =
            {
              Position = { X = field_size; Y = field_size }*0.45
              Velocity = Vector2<_>.Zero
              DryMass = 4.5e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(50.0*180.0)
              Thrust = 5.0e6<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 50.0<_>
              MaxIntegrity = 50.0<_>
              Damage = 1.0e-1<_>/180.0
              WeaponsRange = field_size*0.1
              AI = co{ return () }
            }
          HeadPirate =
            {
              Position ={ X = field_size; Y = field_size }*0.66
              Velocity = Vector2<_>.Zero
              DryMass = 3.0e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(30.0*180.0)
              Thrust = 5.0e5<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 475.0<_>
              MaxIntegrity = 475.0<_>
              Damage = 3.0e-1<_>/180.0
              WeaponsRange = field_size*0.15
              AI = co{ return () }
            }
          Pirate =
            {
              Position ={ X = field_size; Y = field_size }*0.68
              Velocity = Vector2<_>.Zero
              DryMass = 3.0e4<_>
              Fuel = 2.2e6<_>
              MaxFuel = 2.2e6<_>
              FuelBurn = 2.2e6<_>/(30.0*180.0)
              Thrust = 5.0e5<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 275.0<_>
              MaxIntegrity = 275.0<_>
              Damage = 2.0e-1<_>/180.0
              WeaponsRange = field_size*0.15
              AI = co{ return () }
            }
          Cargo =
            {
              Position ={ X = field_size; Y = field_size*0.7 }*0.7
              Velocity = Vector2<_>.Zero
              DryMass = 2.3e6<_>
              Fuel = 3.5e8<_>*0.3
              MaxFuel = 3.5e8<_>
              FuelBurn = 3.5e6<_>/180.0
              Thrust = 3.4e6<_>/180.0
              Force = Vector2<_>.Zero
              Integrity = 600.0<_>
              MaxIntegrity = 600.0<_>
              Damage = 1.0e-3<_>/180.0
              WeaponsRange = field_size*0.1
              AI = co{ return () }
            }
        }
      do s.subPatrol1.AI <- patrol_ai s.subPatrol1 s
      do s.subPatrol2.AI <- patrol_ai s.subPatrol2 s
      do s.HeadPatrol.AI <- patrol_ai s.HeadPatrol s
      do s.Pirate.AI <- pirate_ai s.Pirate s
      do s.HeadPirate.AI <- pirate_ai s.HeadPirate s
      do s.Cargo.AI <- cargo_ai s
      s
    let co_step =
      function
      | Return x -> co{ return x }
      | Yield k -> k
      | ArrowYield k -> k
    let ship_step (s:Ship) =
      do s.Position <- s.Position+s.Velocity*dt
      do s.Velocity <- s.Velocity+dt*s.Force/s.Mass
      do s.Force <- Vector2<_>.Zero
      do s.AI <- co_step (s.AI())
    let simulation_step (s:PoliceChase) =
      do ship_step s.subPatrol1
      do ship_step s.subPatrol2
      do ship_step s.HeadPatrol
      do ship_step s.HeadPirate
      do ship_step s.Pirate
      do ship_step s.Cargo
      if Vector2<_>.Distance(s.HeadPatrol.Position, s.Pirate.Position) < s.HeadPatrol.WeaponsRange then
          do s.Pirate.Integrity <- s.Pirate.Integrity - s.HeadPatrol.Damage*dt
      elif Vector2<_>.Distance(s.HeadPatrol.Position, s.HeadPirate.Position) < s.HeadPatrol.WeaponsRange then
          do s.HeadPirate.Integrity <- s.HeadPirate.Integrity - s.HeadPatrol.Damage*dt
      if Vector2<_>.Distance(s.subPatrol1.Position, s.Pirate.Position) < s.subPatrol1.WeaponsRange then
          do s.Pirate.Integrity <- s.Pirate.Integrity - s.subPatrol1.Damage*dt
      elif Vector2<_>.Distance(s.subPatrol1.Position, s.HeadPirate.Position) < s.subPatrol1.WeaponsRange then
          do s.HeadPirate.Integrity <- s.HeadPirate.Integrity - s.subPatrol1.Damage*dt
      if Vector2<_>.Distance(s.subPatrol2.Position, s.Pirate.Position) < s.subPatrol2.WeaponsRange then
          do s.Pirate.Integrity <- s.Pirate.Integrity - s.subPatrol2.Damage*dt
      elif Vector2<_>.Distance(s.subPatrol2.Position, s.HeadPirate.Position) < s.subPatrol2.WeaponsRange then
          do s.HeadPirate.Integrity <- s.HeadPirate.Integrity - s.subPatrol2.Damage*dt
      if Vector2<_>.Distance(s.Cargo.Position,s.Pirate.Position) < s.Cargo.WeaponsRange then
          do s.Pirate.Integrity <- s.Pirate.Integrity - s.Cargo.Damage*dt
      if Vector2<_>.Distance(s.HeadPatrol.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
          do s.HeadPatrol.Integrity <- s.HeadPatrol.Integrity - s.Pirate.Damage*dt
      elif Vector2<_>.Distance(s.subPatrol1.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
          do s.HeadPatrol.Integrity <- s.subPatrol1.Integrity - s.Pirate.Damage*dt
      elif Vector2<_>.Distance(s.subPatrol2.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
          do s.HeadPatrol.Integrity <- s.subPatrol2.Integrity - s.Pirate.Damage*dt
      elif Vector2<_>.Distance(s.Cargo.Position, s.Pirate.Position) < s.Pirate.WeaponsRange then
          do s.Cargo.Integrity <- s.Cargo.Integrity - s.Pirate.Damage*dt
      if Vector2<_>.Distance(s.HeadPatrol.Position, s.HeadPirate.Position) < s.HeadPirate.WeaponsRange then
          do s.HeadPatrol.Integrity <- s.HeadPatrol.Integrity - s.HeadPirate.Damage*dt
      elif Vector2<_>.Distance(s.subPatrol1.Position, s.HeadPirate.Position) < s.HeadPirate.WeaponsRange then
          do s.HeadPatrol.Integrity <- s.subPatrol1.Integrity - s.HeadPirate.Damage*dt
      elif Vector2<_>.Distance(s.subPatrol2.Position, s.HeadPirate.Position) < s.HeadPirate.WeaponsRange then
          do s.HeadPatrol.Integrity <- s.subPatrol2.Integrity - s.HeadPirate.Damage*dt
      elif Vector2<_>.Distance(s.Cargo.Position, s.HeadPirate.Position) < s.HeadPirate.WeaponsRange then
          do s.Cargo.Integrity <- s.Cargo.Integrity - s.HeadPirate.Damage*dt
    
    let print(s:PoliceChase) =
      do Console.Clear()
      let set_cursor (v:Vector2<_>) =
        Console.SetCursorPosition(
          (((v.X/field_size)*79.0) |> int)-1
            |> max 0 |> min 79,
          ((v.Y/field_size)*23.0) |> int
            |> max 0 |> min 23)
      let set_cursor_on_ship (s:Ship) = set_cursor (s.Position)
      let set_cursor_on_station (s:Station) = set_cursor (s.Position)
      do set_cursor_on_station (s.PoliceStation)
      do Console.Write("Base")
      let ship_fuel (s:Ship) = (9.0*s.Fuel/s.MaxFuel).ToString("#.")
      let ship_integrity (s:Ship) = (9.0*s.Integrity/s.MaxIntegrity).ToString("#.")
      do set_cursor_on_ship (s.HeadPatrol)
      do Console.Write((ship_fuel s.HeadPatrol)+":HPol:"+ (ship_integrity s.HeadPatrol))
      do set_cursor_on_ship (s.subPatrol1)
      do Console.Write((ship_fuel s.subPatrol1)+":Pol1:"+ (ship_integrity s.subPatrol1))
      do set_cursor_on_ship (s.subPatrol2)
      do Console.Write((ship_fuel s.subPatrol2)+":Pol2:"+ (ship_integrity s.subPatrol2))
      do set_cursor_on_ship (s.HeadPirate)
      do Console.Write((ship_fuel s.HeadPirate)+"HPir"+(ship_integrity s.HeadPirate))
      do set_cursor_on_ship (s.Pirate)
      do Console.Write((ship_fuel s.Pirate)+"Pira"+(ship_integrity s.Pirate))
      do set_cursor_on_ship (s.Cargo)
      do Console.Write((ship_fuel s.Cargo)+"Carg"+(ship_integrity s.Cargo))
      do Console.SetCursorPosition(0,0)
      do Thread.Sleep(10)

    let simulation() =
      let s = s0()
      let rec simulation() =
        do print s
        if (s.HeadPatrol.Integrity > 0.0<_> ||
          s.subPatrol2.Integrity > 0.0<_> ||
          s.subPatrol1.Integrity > 0.0<_> ) &&
          (s.HeadPirate.Integrity > 0.0<_> ||
          s.Pirate.Integrity > 0.0<_>) &&
          s.Cargo.Integrity > 0.0<_> then
        do simulation (simulation_step s)
      do simulation()