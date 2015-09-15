  module BallSimulation =

    open System

    let dt = 0.1
    let g = -9.81

    let simulation_step (y,v) =

       let y',v' = (y + v * dt, v + g * dt)

       if y' < 0.0 then
         (0.0,System.Math.Round(-v' * 0.1))
       else
         (y',v')

    let simulation_step_2 (y,v) =

       let y',v' = (y - v * dt, v + g * dt)

       if y' > 9.0 then
         (8.0, System.Math.Round(-v' * 0.1))
       else
         (y',v')

    let simulation_step_3 (y,v) =

       let y',v' = (y - v * dt, v + g * dt)

       if y' > 28.0 then
         (28.0, System.Math.Round(-v' * 0.1))
       else
         (y',v')

    let print_scene (y,v,a,b,c,d,e,f) =
      do Console.Clear()
      let y,v,a,b,c,d,e,f = int y, int v, int a, int b, int c, int d, int e, int f

      for j = 10 downto 0 do
        for i = 0 to 30 do
          if (y+1) = j && i = 15 then
            Console.Write("b")
          elif (a+1) = j && i = 15 then
            Console.Write("a")
          elif (c+1) = i && j = 7 then
            Console.Write("c")
          elif (e+1) = i && j = 3 then
            Console.Write("d")
          elif j = 0 || i = 0 || j = 10 || i = 30 then
            Console.Write("*")
          else
            Console.Write(" ")
        Console.Write("\n")
      Console.Write(y)
      Console.Write(", ")
      Console.Write(v)
      Console.Write("\n")
      Console.Write(a)
      Console.Write(", ")
      Console.Write(b)
      Console.Write("\n")
      Console.Write(c)
      Console.Write(", ")
      Console.Write(d)
      Console.Write("\n")
      Console.Write(e)
      Console.Write(", ")
      Console.Write(f)
      Console.Write("\n")
      ignore(Console.ReadKey())

    let simulation() =

      let rec simulation (y,v,a,b,c,d,e,f) =
        do print_scene (y,v,a,b,c,d,e,f)
        let y',v' = simulation_step (y,v)
        let a',b' = simulation_step_2 (a,b)
        let c',d' = simulation_step (c,d)
        let e',f' = simulation_step_3 (e,f)
        if abs v' > 0.1 || abs b' > 0.1 || abs c' > 0.1 || abs e' > 0.1 then
          do simulation (y',v',a',b',c',d',e',f')

      do simulation (5.0,-2.0,7.0,-2.0,7.0,-2.0,7.0,-2.0)
    
    [<EntryPoint>]
    let main argv = 
        simulation()
        0
