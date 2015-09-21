module Chapter3
  module SmallAsteroidFieldSimulation =

    open System
    open System.Threading

    open Chapter2.Math

    (*
    An asteroid is a record, and it is comprised of a position (in meters), a velocity (in meters per second), a mass (in kilograms) and a name for printing.
    *)
    type Asteroid =
      {
        Position : Vector2<m>
        Velocity : Vector2<m/s>
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

    (*
    We define a function that initializes an asteroid field of an arbitrary number of asteroids:
    *)
    let create_field num_asteroids =
      (*
      We define a useful function, called a linear interpolation ("LERP", or "Linear intERPolation", who knows who picks these names!) that mixes together two values x and y according to
      a coefficient alpha. If alpha is 0 then we get one of the values; if alpha is 1 then we get the other value; if alpha is in between then we get a mix of the two proportional to the value of alpha.
      Notice that to be able to use x and y with units of measure inside the lerp function then we have to explicitly generic) type annotations that say that both x and y have the same unit of measure:
      *)
      let lerp (x:float<'u>) (y:float<'u>) (a:float) = x * a + y * (1.0 - a)
      (*
      We instance a value of type System.Random. F# may access any .Net datatype, even if written in another language such as C# or VB.Net.
      We invoke the constructor of a datatype without the usual keyword "new", which we may optionally provide if so we wished.
      In particular we invoke the constructor of the System.Random datatype to obtain a random number generator:
      *)
      let rand = Random()
      (*
      We can now instance 20 random asteroids.
      We define a list between square brackets.
      The shortest possible list is the empty list: []
      A list of integers with just one element would be [1]
      A list of strings with various elements is written ["hello"; "world"]

      A far more powerful way to manipulate lists would be to use list-comprehension syntax.
      We can define a list with a mixture of loops and the yield keyword.
      The idea is that we write a piece odf code, within brackets. This piece of code is executed, and every time the expression "yield x" is encountered then the value of x is added to the list.
      *)
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
          let vx = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vy = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          (*
          When we are done initializing the values that characterize the asteroid we create it by assigning its fields and then we yield it, adding it to the list.
          We can either use "a" as a name for the asteroid, or a random character to make visually tracking the asteorids much simpler.
          *)
          yield
            {
              Position = { X = x; Y = y }
              Velocity = { X = vx; Y = vy }
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
    let clamp (p:Vector2<_>,v:Vector2<_>) =
      let p,v =
        if p.X < 0.0<_> then
          { p with X = 0.0<_> }, { v with X = -v.X }
        else p,v
      let p,v =
        if p.X > field_size then
          { p with X = field_size }, { v with X = -v.X }
        else p,v
      let p,v =
        if p.Y < 0.0<_> then
          { p with Y = 0.0<_> }, { v with Y = -v.Y }
        else p,v
      let p,v =
        if p.Y > field_size then
          { p with Y = field_size }, { v with Y = -v.Y }
        else p,v
      p,v

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
      (*
      We iterate all asteroids, and apply their velocity, bouncing and the various gravitational forces to each one.
      *)
      [
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
