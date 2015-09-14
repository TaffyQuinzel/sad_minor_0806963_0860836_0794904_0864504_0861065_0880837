module Chapter1
  module BallSimulation =
    open System
    open System.Threading
    open Chapter2.Math
//    open Chapter3.SmallAsteroidFieldSimulation

    let dt = 0.5<s>

    let clamp (p: Vector2<_>, v: Vector2<_>) =
      let p, v =
        if p.X < 1.0<_> then { p with X = 1.0<_> }, { v with X = 0.75 * -v.X }
        else p, v
      let p, v =
        if p.X > 78.0<_> then { p with X = 78.0<_> }, { v with X = 0.75 * -v.X }
        else p, v
      let p,v =
        if p.Y < 1.0<_> then { p with Y = 1.0<_> },{ v with Y = 0.75 * -v.Y }
        else p,v
      let p,v =
        if p.Y > 22.0<_> then { p with Y = 22.0<_> },{ v with Y = 0.75 * -v.Y }
        else p,v
      p,v

    type Ball = 
      {
        Position: Vector2<m>
        Velocity: Vector2<m/s>
        Force: Vector2<kg m / s^2>
        Name: string
      }
      
      static member step (b: Ball) =
        let vx', vy' = (b.Velocity.X + dt * b.Force.X / 1.0<kg>), (b.Velocity.Y + dt * b.Force.Y / 1.0<kg>)
        let x', y' =  b.Position.X + dt * vx', b.Position.Y + dt * vy'
        let p', v' = clamp ({ X = x'; Y = y' }, { X = vx'; Y = vy' })

        { 
          b with 
            Position = p' 
            Velocity = v'
        }

    let simulation_step (balls: Ball list) =
      let rec step_ (ball_list: Ball list) =
        match ball_list with
        | b :: bs -> (Ball.step b) :: step_ bs
        | [] -> []
      step_ balls


    let print_scene (b: Ball list) = 
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
      for ball in b do
        let x = ball.Position.X |> int
        let y = ball.Position.Y |> int
        do Console.SetCursorPosition(x, y)
        do Console.Write(ball.Name)
      ignore(Console.ReadKey())
  
    let simulation() =
      let rec simulation (b: Ball list) =
        do print_scene (b)
        let b' = simulation_step b
        do simulation(b')
      let balls = 
        [
          {
            Position = {X = 5.0<m>; Y = 4.0<m>}
            Velocity = {X = 0.0<m/s>; Y = 0.0<m/s>}
            Force = { X = 2.0<N>; Y = 1.0<N> }
            Name = "1"
          }
          {
            Position = {X = 8.0<m>; Y = 17.0<m>}
            Velocity = {X = 0.0<m/s>; Y = 0.0<m/s>}
            Force = { X = 1.0<N>; Y = -3.0<N> }
            Name = "2"
          }
          {
            Position = {X = 49.0<m>; Y = 20.0<m>}
            Velocity = {X = 0.0<m/s>; Y = 0.0<m/s>}
            Force = { X = -1.5<N>; Y = 1.0<N> }
            Name = "3"
          }
          {
            Position = {X = 23.0<m>; Y = 22.0<m>}
            Velocity = {X = 0.0<m/s>; Y = 0.0<m/s>}
            Force = { X = -2.5<N>; Y = -2.0<N> }
            Name = "4"
          }
          {
            Position = {X = 1.0<m>; Y = 15.0<m>}
            Velocity = {X = 0.0<m/s>; Y = 0.0<m/s>}
            Force = { X = 2.0<N>; Y = 0.0<N> }
            Name = "5"
          }
        ]
      do simulation balls