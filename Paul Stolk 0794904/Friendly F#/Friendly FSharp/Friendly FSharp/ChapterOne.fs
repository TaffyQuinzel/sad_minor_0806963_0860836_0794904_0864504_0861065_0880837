namespace ChapterOne

  module BallSimulation =
    open System
    
      
    let dt = 0.1
    let gravity = -9.81

    let simulation_step (currentPosition, velocity) =
      let currentPosition', velocity' = (currentPosition + velocity * dt, velocity + gravity * dt)
      if currentPosition' < 0.0 then
        (0.0, -velocity' * 0.7)
      else
        (currentPosition', velocity')

    let print_scene (currentPosition, velocity) = 
      do Console.Clear()
      let currentPosition, velocity = int currentPosition, int velocity
      for j = 10 downto 0 do
        for i = 0 to 30 do
          if (currentPosition + 1) = j && i = 15 then
            Console.Write("b")
          elif j = 0 || i = 0 || j = 10 || i = 30 then
            Console.Write("*")
          else
            Console.Write(" ")
        Console.Write("\n")
      ignore(Console.ReadKey())
  
    let simulation() =
      let rec simulation (currentPosition, velocity) =
        do print_scene (currentPosition,  velocity)
        let currentPosition', velocity'= simulation_step (currentPosition, velocity)
        if abs velocity' > 0.1 then
          do simulation(currentPosition', velocity')
      do simulation (5.0, -2.0)