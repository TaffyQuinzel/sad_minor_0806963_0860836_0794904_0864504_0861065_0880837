namespace ChapterTwo

  module RocketSimulation =
    open System
    open System.Threading
    open Global.Math

    type PhysicalEntity =
      {
        Position: Vector2<meters>
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
        FuelBurn: float<kg/seconds>
        Thrust:   float<Newton>
      }

      member this.Mass: float<kg> = this.DryMass + this.Fuel

    type Rocket =
      {
        Body:     PhysicalEntity
        BaseMass: float<kg>
        Velocity: Vector2<meters/seconds>
        Stage1:   Option<Stage>
        Stage2:   Option<Stage>
        Stage3:   Option<Stage>
      }
      
    type ApolloMission =
      {
        Earth:  CelestialBody
        Moon:   CelestialBody
        Rocket: Rocket
      }

    let earth_radius = 6.37e6<meters>
    let earth_mass = 5.97e24<kg>
    let moon_mass = 7.35e22<kg>
    let dt = 60.0<seconds>
    let G = 6.67e-11<meters^3*kg^-1*seconds^-2>

    let m0: ApolloMission =
      {
        Earth = 
          {
            Body = 
              {
                Position = { X = 0.0<meters>; Y = 0.0<meters> }
                Mass = earth_mass
              }
            Name = "E"
          }
        Moon =
          {
            Body =
              {
                Position = { X = earth_radius*60.0; Y = 0.0<meters> }
                Mass = moon_mass
              }
            Name = "M"
          }
        Rocket = 
          let stage1 = 
            {
              DryMass = 1.31e5<kg>
              Fuel = 2.17e6<kg>
              FuelBurn = 8.25e3<kg/seconds>
              Thrust = 3.4e7<Newton>*5.0
            }
          let stage2 =
            {
              DryMass = 3.6e4<kg>
              Fuel = 4.4e5<kg>
              FuelBurn = 1.05e3<kg/seconds>
              Thrust = 4.4e6<Newton>*5.0
            }
          let stage3 =
            {
              DryMass = 1.1e4<kg>
              Fuel = 1.09e5<kg>
              FuelBurn = 2.59e2<kg/seconds>
              Thrust = 1.0e6<Newton>
            }
          let base_mass = 4.5e4<kg>
          {
            Body = 
              {
                Position = { X = earth_radius; Y = 11.0<meters> }
                Mass = 
                  base_mass + stage1.Mass + stage2.Mass + stage3.Mass
              }
            BaseMass = base_mass
            Velocity = Vector2<meters/seconds>.Zero
            Stage1 = Some(stage1)
            Stage2 = Some(stage2)
            Stage3 = Some(stage3)
          }
      }
    let simulation_step (m: ApolloMission) =
      let r = m.Rocket
      let F_body (b: CelestialBody) =
        let dir = b.Body.Position - r.Body.Position
        let dist = dir.Length + 1.0<meters>
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
        | _ -> Vector2<Newton>.Zero, r
      let F =
        let F_earth = (F_body m.Earth)
        let F_moon = (F_body m.Moon)
        F_earth + F_moon + F_engine
      let r =
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
                    { p with X = max (p.X) earth_radius }
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
          ((b.Position.X / 4.0e8<meters>) * 78.0 + 1.0) |> int,
          (b.Position.X / 4.0e8<meters> + 11.0) |> int)
      do set_cursor_on_body m.Earth.Body
      do Console.Write(m.Earth.Name)
      do set_cursor_on_body m.Moon.Body
      do Console.Write(m.Moon.Name)
      do set_cursor_on_body m.Rocket.Body
      do Console.Write("R")
      do Thread.Sleep(100)

    let simulation() = 
      let rec simulation m =
        do print_scene m
        let m' = simulation_step m
        if Vector2<_>.Distance(m'.Moon.Body.Position, m'.Rocket.Body.Position) > 1.7e6<meters> then
          do simulation m'
      do simulation m0