module Chapter4
  module LargeAsteroidFieldSimulation =
    open System
    open System.Threading
    open Chapter2.Math
    open Chapter3.SmallAsteroidFieldSimulation
    open Trees

    type Asteroid3D = 
      {
        Position: Vector3<m>
        Velocity: Vector3<m/s>
        Mass: float<kg>
        Name: string
      }

    let create_field num_asteroids: Asteroid3D list =
      let lerp (x: float<'u>) (y: float<'u>) (a: float) =
        x * a + y * (1.0 - a)
      let rand = Random()
      [
        for i = 1 to num_asteroids do
          let m = (lerp earth_mass moon_mass (rand.NextDouble())) * 1.0e-4
          let x = lerp 0.0<m> field_size (rand.NextDouble())
          let y = lerp 0.0<m> field_size (rand.NextDouble())
          let z = lerp 0.0<m> field_size (rand.NextDouble())
          let vx = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vy = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vz = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          yield
            {
              Position = { X = x; Y = y; Z = z }
              Velocity = { X = vx; Y = vy; Z = vz }
              Mass = m
              Name = "a"
            }
      ]

    let f0 = create_field 10

    type Barycenter =
      {
        Position: Vector3<m>
        Mass: float<kg>
      }

      member this.ToAsteroid: Asteroid3D =
        {
          Position = this.Position
          Mass = this.Mass
          Name = ""
          Velocity = Vector3<_>.Zero
        }

      static member (+/) (b1: Barycenter, b2: Barycenter) =
        let new_mass = b1.Mass + b2.Mass
        if new_mass < 0.01<_> then
          {
            Position = Vector3<_>.Zero
            Mass = 0.0<_>
          }
        else
          {
            Position = (b1.Position * b1.Mass + b2.Position * b2.Mass) / new_mass
            Mass = new_mass
          }

      static member OfAsteroidList (l: Asteroid3D list) =
        let positions_weighted_sum = Seq.sum (Seq.map (fun (a: Asteroid3D) -> a.Position * a.Mass) l)
        let masses_sum = Seq.sumBy (fun (a: Asteroid3D) -> a.Mass) l
        {
          Position =
            if masses_sum > 0.01<_> then
              positions_weighted_sum / masses_sum
            else Vector3<_>.Zero
          Mass = masses_sum
        }

    let force (a: Asteroid3D, a': Asteroid3D) =
      let dir = a'.Position - a.Position
      let dist = dir.Length + 1.0<m>
      G * a.Mass * a'.Mass * dir / (dist * dist * dist)

    let clamp (p: Vector3<_>, v: Vector3<_>) =
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
      let p,v =
        if p.Z < 0.0<_> then { p with Z = 0.0<_> },{ v with Z = -v.Z }
        else p,v
      let p,v =
        if p.Z > field_size then { p with Z = field_size },{ v with Z = -v.Y }
        else p,v
      p,v

    let fast_simulation_step (asteroids: Asteroid3D list) =
      let empty_tree =
        OctTree<_,_,_>.mk_empty
          { Min = Vector3<_>.Zero; Size = { X = field_size / 8.0; Y = field_size / 8.0; Z = field_size / 8.0 } }
          { Min = Vector3<_>.Zero; Size = { X = field_size; Y = field_size; Z = field_size } }
      let tree =
        List.fold (fun t a -> 
          OctTree<_,_,_>.insert (fun (a: Asteroid3D) -> a.Position) a t)
          empty_tree asteroids
      let tree = 
        OctTree<_,_,_>.fold (fun a b c d e f g h-> ((a +/ b) +/ (c +/ d)) +/ ((e +/ f) +/ (g +/ h))) 
          Barycenter.OfAsteroidList tree
      let local_forces (others: Barycenter) asteroid_group =
        [
          for a in asteroid_group do
            let forces =
              seq{
                for a' in asteroid_group do
                  if a' <> a then
                    yield force(a, a')
              }
            let F_local = Seq.sum forces
            let F = F_local + force(a, others.ToAsteroid)
            let p', v' = clamp(a.Position, a.Velocity)
            yield 
              { 
                a with
                  Position = p' + dt * v'; 
                  Velocity = v' + dt * F / a.Mass 
              }
        ]
      let rec traverse (others: Barycenter) =
        function
        | OctTree.Leaf(r, a, b) -> OctTree.Leaf(r, local_forces others a, b)
        | OctTree.Node(r, q111, q112, q121, q122, q211, q212, q221, q222, b) ->
          let q111' =
            traverse (others +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q111
          let q112' =
            traverse (others +/ q111.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q112
          let q121' =
            traverse (others +/ q111.State +/ q112.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q121
          let q122' =
            traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q122
          let q211' =
            traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q212.State +/ q221.State +/ q222.State) q211
          let q212' =
            traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q221.State +/ q222.State) q212
          let q221' =
            traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q222.State) q221
          let q222' =
            traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State) q222
          OctTree.Node(r, q111', q112', q121', q122', q211', q212', q221', q222', b)
      let list = (traverse { Position = Vector3<_>.Zero; Mass = 0.0<_> } tree).ToList
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
    
    let s = Diagnostics.Stopwatch()
    let print_framerate3D (asteroids: Asteroid3D list) =
      do Console.Clear()
      let dt = s.Elapsed
      let dt = 1.0 / dt.TotalSeconds
      do Console.WriteLine(dt.ToString("0#.#"))

    let print_scene (asteroids: Asteroid3D list) =
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
      let set_cursor_on_body (b: Asteroid3D) =
        Console.SetCursorPosition(
          ((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int,
          ((b.Position.Y / 4.0e8<m>) * 23.0 + 1.0) |> int)
      for a in asteroids do
        do set_cursor_on_body a
        let z = ((a.Position.Z / 4.0e8<m>) * 8.0 + 1.0) |> int
        do Console.Write(z)
      Console.SetCursorPosition(0, 24)
      Console.Write(asteroids.Length)
      do Thread.Sleep(100)

    let base_simulation (print_scene: (Asteroid3D list -> unit)) (simulation_step: (Asteroid3D list -> Asteroid3D list)) =
      let rec simulation m =
        do print_scene m
        do s.Reset()
        do s.Start()
        let m' = simulation_step m
        do s.Stop()
        do simulation m'
      do simulation f0

    let fast_simulation() =
      base_simulation print_scene fast_simulation_step
    let fast_simulation_framerate() =
      base_simulation print_framerate3D fast_simulation_step
