namespace ChapterOne

  module BallSimulation =
    open System
    
    let dt = 0.1
    let gravity = -9.81

    let simulation_step (currentPosition, velocity, gravityDirection ) =
      let currentPosition', velocity' = (currentPosition + velocity * dt, velocity + (gravity * gravityDirection) * dt)
      if currentPosition' < 0.0 then
        (0.0, -velocity' * 0.7)
      elif(currentPosition' > 30.0) then
        (30.0, -velocity' * 0.7)
      else
        (currentPosition', velocity')

    let print_scene (ball1, ball2, ball3, ball4) = 
      do Console.Clear()
      let ball1, ball2, ball3, ball4  = int ball1, int ball2, int ball3, int ball4 
      for j = 30 downto 0 do
        for i = 0 to 30 do
          if ((ball1 + 1) = j && i = 15 ) || ( (ball2 + 1) = j && i = 15) || ((ball3 + 1) = i && j = 5) || ((ball4 + 1) = i && j = 5) then
            Console.Write("b")
          elif j = 0 || i = 0 || j = 30 || i = 30 then
            Console.Write("*")
          else
            Console.Write(" ")
        Console.Write("\n")
      ignore(Console.ReadKey())
  
    let simulation() =
      let rec simulation (ball1, velocity1, ball2, velocity2, ball3, velocity3, ball4, velocity4) =
        do print_scene (ball1,ball2,ball3,ball4)
        let ball1', velocity1'= simulation_step (ball1, velocity1, 1.0 )
        let ball2', velocity2'= simulation_step (ball2, velocity2, -1.0)
        let ball3', velocity3'= simulation_step (ball3, velocity3, 1.0)
        let ball4', velocity4'= simulation_step (ball4, velocity4, -1.0)
        if abs velocity1' > 0.1 || abs velocity2' > 0.1 || abs velocity3' > 0.1 || abs velocity4' > 0.1 then
          do simulation(ball1', velocity1',ball2', velocity2',ball3', velocity3',ball4', velocity4')
      do simulation (6.0, -2.0, 6.0, 2.0, 3.0, -2.0, 3.0, 2.0)