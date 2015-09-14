module Chapter3
  module SmallAsteroidFieldSimulation = 
    open System
    open System.Threading
    open Chapter2.Math

    type Asteroid = 
      {
        Position: Vector2<m>
        Velocity: Vector2<m/s>
        Mass: float<kg>
        Name: string
      }

    let dt = 60.0<s>
    let G = 6.67e-11<m^3*kg^-1*s^-2>

    let earth_radius = 6.37e6<m>
    let field_size = earth_radius * 60.0
    let max_velocity = 2.3e4<m/s>
    let earth_mass = 5.97e24<kg>
    let moon_mass = 7.35e22<kg>

    let create_field num_asteroids =
      let lerp (x: float<'u>) (y: float<'u>) (a: float) =
        x * a + y * (1.0 - a)
      let rand = Random()
      [
        for i = 1 to num_asteroids do
          let m = (lerp earth_mass moon_mass (rand.NextDouble())) * 1.0e-4
          let x = lerp 0.0<m> field_size (rand.NextDouble())
          let y = lerp 0.0<m> field_size (rand.NextDouble())
          let vx = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vy = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          yield
            {
              Position = { X = x; Y = y }
              Velocity = { X = vx; Y = vy }
              Mass = m
              Name = "a"
            }
      ]

    let clamp (p: Vector2<_>, v: Vector2<_>) =
      let p, v =
        if p.X < 0.0<_> then { p with X = 0.0<_> }, { v with X = -v.X }
        else p, v
      let p, v =
        if p.X > field_size then { p with X = field_size }, { v with X = -v.X }
        else p, v
      let p,v =
        if p.Y < 0.0<_> then { p with Y = 0.0<_> },{ v with Y = -v.Y }
        else p,v
      let p,v =
        if p.Y > field_size then { p with Y = field_size },{ v with Y = -v.Y }
        else p,v
      p,v

    let force (a: Asteroid, a': Asteroid) =
      let dir = a'.Position - a.Position
      let dist = dir.Length + 1.0<m>
      G * a.Mass * a'.Mass * dir / (dist * dist * dist)

    let simulation_step (asteroids: Asteroid list) =
      let list = 
        [
          for a in asteroids do
            let forces =
              [
                for a' in asteroids do
                  if a' <> a then
                    yield force (a, a')
              ]
            let F = List.sum forces
            let p', v' = clamp(a.Position, a.Velocity)
            yield
              {
                a with
                  Position = p' + dt * v'
                  Velocity = v' + dt * F / a.Mass
              }
        ]
      if (Console.KeyAvailable) then
        let ukey = Console.ReadKey(true)
        match ukey.Key with
        | ConsoleKey.D1 | ConsoleKey.NumPad1 -> list @ create_field 1
        | ConsoleKey.D2 | ConsoleKey.NumPad2 -> list @ create_field 2
        | ConsoleKey.D3 | ConsoleKey.NumPad3 -> list @ create_field 3
        | ConsoleKey.D4 | ConsoleKey.NumPad4 -> list @ create_field 4
        | ConsoleKey.D5 | ConsoleKey.NumPad1 -> list @ create_field 5
        | ConsoleKey.D6 | ConsoleKey.NumPad6 -> list @ create_field 6 
        | ConsoleKey.D7 | ConsoleKey.NumPad7 -> list @ create_field 7
        | ConsoleKey.D8 | ConsoleKey.NumPad8 -> list @ create_field 8
        | ConsoleKey.D9 | ConsoleKey.NumPad9 -> list @ create_field 9
        | ConsoleKey.D0 | ConsoleKey.NumPad0 -> list @ create_field 10
        | _ -> list
      else 
        list
          
    let print_scene (asteroids: Asteroid list) =
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
          ((b.Position.Y / 4.0e8<m>) * 23.0 + 1.0) |> int)
      for a in asteroids do
        do set_cursor_on_body a
        do Console.Write(a.Name)
      Console.SetCursorPosition(0, 24)
      Console.Write(asteroids.Length)
      do Thread.Sleep(100)
    
    let f0 = create_field 10

    let simulation() =
      let rec simulation m =
        do print_scene m
        let m' = simulation_step m
        do simulation m'
      do simulation (create_field 20)
