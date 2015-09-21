
module Chapter3
  module SmallAsteroidFieldSimulation =

    open System
    open System.Threading

    open Chapter2.Math


    type Vector3<[<Measure>] 'a> =
      {
        X : float<'a>
        Y : float<'a>
        Z : float<'a>
      }

      static member Zero : Vector3<'a> = { X = 0.0<_>; Y = 0.0<_>; Z = 0.0<_> }

      static member ( + ) (v1:Vector3<'a>,v2:Vector3<'a>):Vector3<'a> = { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
      static member ( - ) (v1:Vector3<'a>,v2:Vector3<'a>):Vector3<'a> = { X = v1.X - v2.X; Y = v1.Y - v2.Y; Z = v1.Z - v2.Z }
      static member ( * ) (v:Vector3<'a>,f:float<'b>):Vector3<'a * 'b> = { X = v.X * f; Y = v.Y * f; Z = v.Z * f }
      static member ( * ) (f:float<'b>,v:Vector3<'a>):Vector3<'b * 'a> = { X = f * v.X; Y = f * v.Y; Z = f * v.Z }
      static member ( / ) (v:Vector3<'a>,f:float<'b>):Vector3<'a / 'b> = v * (1.0 / f)

      member this.Length : float<'a> = sqrt((this.X * this.X + this.Y * this.Y))
    (*
    An asteroid is a record, and it is comprised of a position (in meters), a velocity (in meters per second), a mass (in kilograms) and a name for printing.
    *)
    type Asteroid =
      {
        Position : Vector3<m>
        Velocity : Vector3<m/s>
        Mass     : float<kg>
        Name     : string
      }

    (*
    We declare a set of constants that characterize our simulation.
    dt is the amount of time we simulate at each step, G is the gravitational constant and the various other constants are used to define (large) asteroids with mass between the Earth and Moon masses.

    The size of the asteroid field is quite small: it fits entirely within the distance between the Earth and the Moon.
    This way the gravitational forces should be unrealistically intense but quite spectacular to look at!
    *)
    let dt = 60.0<s>
    let G = 6.67e-11<m^3 * kg^-1 * s^-2>

    let earth_radius = 6.37e6<m>
    let field_size = earth_radius * 60.0
    let max_velocity = 2.3e4<m/s>
    let earth_mass  = 5.97e24<kg>
    let moon_mass = 7.35e22<kg>

    let lerp (x:float<'u>) (y:float<'u>) (a:float) = x * a + y * (1.0 - a)

    let create_field num_asteroids =

      let rand = Random()

      [
        for i = 1 to num_asteroids do
          (*
          To create an asteroid, we generate a random mass, a random position and a random velocity.
          The mass is between the Earth and Moon masses.
          The position is somewhere within the field.
          The velocity is between -max_velocity and +max_velocity, where max_velocity is what it would take to go from one side to the other of the entire field in a reasonable amount of time (a few minutes, that is a few ticks of the simulation).
          *)
          let m = (lerp earth_mass moon_mass (rand.NextDouble())) * 1.0e-4
          let x = lerp 0.0<m> field_size (rand.NextDouble())
          let y = lerp 0.0<m> field_size (rand.NextDouble())
          let z = lerp 0.0<m> field_size (rand.NextDouble())
          let vx = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vy = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vz = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          (*
          When we are done initializing the values that characterize the asteroid we create it by assigning its fields and then we yield it, adding it to the list.
          We can either use "a" as a name for the asteroid, or a random character to make visually tracking the asteorids much simpler.
          *)
          yield
            {
              Position = { X = x; Y = y; Z = z }
              Velocity = { X = vx; Y = vy; Z = vz }
              Mass     = m
              Name     = "a" // string(char((int 'a') + rand.Next(27)))
            }

      ]

    (*
    The initial asteroid field contains 20 randomized asteroids.
    *)
    let f0 = create_field 20

    (*
    We create two auxiliary functions that we will use in a similar simulation in the next chapter.

    We start with an auxiliary function that forces asteroids to bounce on the field borders.
    If the position exits from the field borders, then we force the asteroid back inside and modify its velocity towards the inside of the field:
    *)
    let clamp (position:Vector3<_>,velocity:Vector3<_>) =
      let position, velocity =
        if position.X < 0.0<_> then
          { position with X = 0.0<_> }, { velocity with X = -velocity.X }
        else position,velocity
      let position,velocity =
        if position.X > field_size then
          { position with X = field_size }, { velocity with X = -velocity.X }
        else position,velocity
      let position,velocity =
        if position.Y < 0.0<_> then
          { position with Y = 0.0<_> }, { velocity with Y = -velocity.Y }
        else position,velocity
      let position,velocity =
        if position.Y > field_size then
          { position with Y = field_size }, { velocity with Y = -velocity.Y }
        else position,velocity
      let position, velocity =
        if position.Z < 0.0<_> then
          { position with Z = 0.0<_> }, { velocity with Z = -velocity.Z }
        else position,velocity
      let position,velocity =
        if position.Z > field_size then
          { position with Z = field_size }, { velocity with Z = -velocity.Z }
        else position,velocity
      position,velocity

    (*
    We define another auxiliary function that computes the force between two asteroids a and a' with the known equation:
    *)
    let force (a:Asteroid,a':Asteroid) =
      let dir = a'.Position - a.Position
      let dist = dir.Length + 1.0<m>
      G * a.Mass * a'.Mass * dir / (dist * dist * dist)

    (*
    A step of the simulation updates each asteroid, according to the following rules:
    - an asteroids keeps moving along its velocity
    - an asteroid bounces on the borders of the field
    - an asteroid is subject to gravitational attraction from all the other asteroids
    *)
    let simulation_step (asteroids:Asteroid list) =
      let asteroids' = [
        for a in asteroids do
          (*
          We find the list of all the forces that the other asteroids apply on a. We check if two asteroids are the same with the <> operator:
          *)
          let forces =
               [
                 for a' in asteroids do
                   if a' <> a then
                     yield force(a,a')
               ]
          (*
          The final force that is applied on the current asteroid a is the sum of all the forces from the various asteroids:
          *)
          let F = List.sum forces
          (*
          We compute the effects of bouncing with the clamp function, and then we yield the updated asteroid by increasing its position by its velocity and its velocity by its acceleration.
          *)
          let p',v' = clamp(a.Position,a.Velocity)
          yield
            {
              a with
                  Position = p' + dt * v'
                  Velocity = v' + dt * F / a.Mass
            }
      ]
      if (Console.KeyAvailable) then
        System.Console.ReadKey() |> ignore
        asteroids' @ create_field 1
      else 
        asteroids'

    (*
    The printing and simulation functions are almost the same we have seen in Chapter 2.
    *)
    let print_scene (asteroids:Asteroid list) =
      do Console.Clear()
      for i = 0 to 79 do
        Console.SetCursorPosition(i, 0)
        Console.Write("*")
        Console.SetCursorPosition(i, 23)
        Console.Write("*")
      for j = 0 to 23 do
        Console.SetCursorPosition(0,j)
        Console.Write("*")
        Console.SetCursorPosition(79,j)
        Console.Write("*")
      let set_cursor_on_body b =
        Console.SetCursorPosition(((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int, ((b.Position.Y / 4.0e8<m>) * 23.0 + 1.0) |> int)
      for a in asteroids do
        do set_cursor_on_body a
        do Console.Write(a.Name)
        //do Console.Write(a.Position.Z)
      Console.SetCursorPosition(0, 0);
      do Console.Write(asteroids.Length);
      do Thread.Sleep(100)

    let simulation() =
      let rec simulation m =
        do print_scene m
        let m' = simulation_step m
        do simulation m'
      do simulation f0

    [<EntryPoint>]
    let main argv = 
       simulation()
       0