// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
  //  do ChapterOne.BallSimulation.simulation()
 //   do ChapterTwo.RocketSimulation.simulation()
    //do ChapterThree.SmallAsteroidFieldSimulation.simulation()
 //   do ChapterFour.LargeAsteroidFieldSimulation.slow_simulation()
   // do ChapterFour.LargeAsteroidFieldSimulation.slow_simulation_framerate()
    //do ChapterFour.LargeAsteroidFieldSimulation.fast_simulation()
 //   do ChapterFour.LargeAsteroidFieldSimulation.fast_simulation_framerate()
   // do ChapterFive.PoliceChase.simulation()
    printfn "%A" Monads.sum
    do System.Threading.Thread.Sleep(9000)
    0 // return an integer exit code

