module KarateIntList.chopTest
  open NUnit.Framework
  open FsUnit
  open KarateChop

  [<TestFixture>]
  type GetTest() =
    [<Test>]
    member this.GetEmptyList() = 
      let list = Empty
      (fun () -> list.Get(2) |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    member this.GetFirstElement() =
      let list = Cons(1, Cons(3, Cons(5, Empty)))
      list.Get(0) |> should equal 1

    [<Test>]
    member this.GetNotFirstElement() = 
      let list = Cons(1, Cons(3, Cons(5, Empty)))
      list.Get(1) |> should equal 3
      list.Get(2) |> should equal 5

  [<TestFixture>]
  type ChopTest() =
    
    [<Test>]
    member this.SearchElementInEmptyList() =
      IntList.chop(3, Empty) |> should equal -1

    [<Test>]
    member this.SearchExistingElementInNonEmptyList() =
      let list = Cons(1, Cons(3, Cons(5, Empty)))
      IntList.chop(1, list) |> should equal 0
      IntList.chop(3, list) |> should equal 1
      IntList.chop(5, list) |> should equal 2
    
    [<Test>]
    member this.SearchMissingElementInNonEmptyList() =
      let list = Cons(1, Cons(3, Cons(5, Empty)))
      IntList.chop(4, list) |> should equal -1