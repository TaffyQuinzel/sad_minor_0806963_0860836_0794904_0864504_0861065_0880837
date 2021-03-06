﻿module Chapter2
  module Math =

    [<Measure>]
    type m

    [<Measure>]
    type kg

    [<Measure>]
    type s

    [<Measure>]
    type N = kg * m / s^2

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

  module RocketSimulation =
    open System
    open System.Threading
    open Math

    type PhysicalEntity =
      {
        Position: Vector2<m>
        Mass:     float<kg>
      }

    type CelestialBody = 
      {
        Body: PhysicalEntity
        Name: string      
      }

    type Stage = 
      {
        DryMass:  float<kg>
        Fuel:     float<kg>
        FuelBurn: float<kg/s>
        Thrust:   float<N>
      }

      member this.Mass: float<kg> = this.DryMass + this.Fuel

    type Rocket =
      {
        Body:     PhysicalEntity
        BaseMass: float<kg>
        Velocity: Vector2<m/s>
        Stage1:   Option<Stage>
        Stage2:   Option<Stage>
        Stage3:   Option<Stage>
      }
      
    type ApolloMission =
      {
        Earth:  CelestialBody
        Moon:   CelestialBody
        DustBelt: float<m>
        Rocket: Rocket
      }

    let earth_radius = 6.37e6<m>
    let earth_mass = 5.97e24<kg>
    let moon_mass = 7.35e22<kg>
    let dt = 30.0<s>
    let G = 6.67e-11<m^3*kg^-1*s^-2>

    let m0: ApolloMission =
      {
        Earth = 
          {
            Body = 
              {
                Position = { X = 0.0<m>; Y = 0.0<m> }
                Mass = earth_mass
              }
            Name = "E"
          }
        Moon =
          {
            Body =
              {
                Position = { X = earth_radius*60.0; Y = 0.0<m> }
                Mass = moon_mass
              }
            Name = "M"
          }
        DustBelt = 10.0e7<m>
        Rocket = 
          let stage1 = 
            {
              DryMass = 1.31e5<kg>
              Fuel = 2.17e6<kg>
              FuelBurn = 8.25e3<kg/s>
              Thrust = 3.4e7<N>*5.0
            }
          let stage2 =
            {
              DryMass = 3.6e4<kg>
              Fuel = 4.4e5<kg>
              FuelBurn = 1.05e3<kg/s>
              Thrust = 4.4e6<N>*5.0
            }
          let stage3 =
            {
              DryMass = 1.1e4<kg>
              Fuel = 1.09e5<kg>
              FuelBurn = 2.59e2<kg/s>
              Thrust = 1.0e6<N>
            }
          let base_mass = 4.5e4<kg>
          {
            Body = 
              {
                Position = { X = earth_radius; Y = 11.0<m> }
                Mass = 
                  base_mass + stage1.Mass + stage2.Mass + stage3.Mass
              }
            BaseMass = base_mass
            Velocity = Vector2<m/s>.Zero
            Stage1 = Some(stage1)
            Stage2 = Some(stage2)
            Stage3 = Some(stage3)
          }
      }
    let simulation_step (m: ApolloMission) =
      let r = m.Rocket
      let F_body (b: CelestialBody) =
        let dir = b.Body.Position - r.Body.Position
        let dist = dir.Length + 1.0<m>
        G * b.Body.Mass * r.Body.Mass * dir / (dist * dist * dist)
      let F_engine, r =
        let stage_step s =
          if s.Fuel <= s.FuelBurn * dt then
            Console.Beep()
            None
          else
            Some({ s with Fuel = s.Fuel - s.FuelBurn * dt})
        let dir = Vector2<_>.Normalize(m.Moon.Body.Position - r.Body.Position)
        match r.Stage1, r.Stage2, r.Stage3 with
        | Some s, _, _ -> 
          dir * s.Thrust, { r with Stage1 = stage_step s }
        | _, Some s, _ -> 
          dir * s.Thrust, { r with Stage2 = stage_step s }
        | _, _, Some s -> 
          dir * s.Thrust, { r with Stage3 = stage_step s }
        | _ -> Vector2<N>.Zero, r
      let F =
        let F_earth = (F_body m.Earth)
        let F_moon = (F_body m.Moon)          
        let f = F_earth + F_moon + F_engine
        f
        
      let r =
        let y = 
          let y' = r.Body.Position.Y
          if (Console.KeyAvailable) then
            let ukey = Console.ReadKey(true)
            if ukey.Key = ConsoleKey.UpArrow then
              y' - 4.0e8<m>
            elif ukey.Key = ConsoleKey.DownArrow then
              y' + 4.0e8<m>
            else y'
          else
            y'
        let stage_mass =
          function
          | None -> 0.0<_>
          | Some s -> s.DryMass + s.Fuel
        {
          r with
            Body = 
              {
                r.Body with
                  Position =
                    let p = r.Body.Position + r.Velocity * dt
                    { p with X = max (p.X) earth_radius; Y = y }
                  Mass = 
                    r.BaseMass + stage_mass r.Stage1 + stage_mass r.Stage2 + stage_mass r.Stage3
              }
            Velocity = r.Velocity + (F / r.Body.Mass) * dt
          }
      { m with Rocket = r }

    let print_scene (m: ApolloMission) =
      do Console.Clear()
      for i = 0 to 79 do
        Console.SetCursorPosition(i, 0)
        Console.Write("*")
        Console.SetCursorPosition(i, 23)
        Console.Write("*")
      for j = 0 to 23 do
        Console.SetCursorPosition(0, j)
        Console.Write("*")
        Console.SetCursorPosition(79, j)
        Console.Write("*")
      let set_cursor_on_body b =
        Console.SetCursorPosition(
          ((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int,
          (b.Position.Y / 4.0e8<m> + 11.0) |> int)
      do set_cursor_on_body m.Earth.Body
      do Console.Write(m.Earth.Name)
      do set_cursor_on_body m.Moon.Body
      do Console.Write(m.Moon.Name)
      do set_cursor_on_body m.Rocket.Body
      do Console.Write("R")
      do Console.SetCursorPosition(0, 24)
      do Console.Write(m.Rocket.Body.Position.Y |> int)
      do Thread.Sleep(100)

    let simulation() = 
      let rec simulation m =      
        do print_scene m
        let m' = simulation_step m
        if Vector2<_>.Distance(m'.Moon.Body.Position, m'.Rocket.Body.Position) > 1.7e6<m> then
          do simulation m'
      do simulation m0