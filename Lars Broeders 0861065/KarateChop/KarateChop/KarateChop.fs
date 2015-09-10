module KarateChop
  type IntList = 
    | Cons of int*IntList
    | Empty
    
    static member (++) (x: int, xs: IntList): IntList =
      Cons(x, xs)

    member this.Get(index: int): int =
      let rec get (index: int, list: IntList): int =
        match list with
        | Cons(x, xs) ->
          if index = 0 then
            x
          else
            get(index - 1, xs)
        | Empty -> failwith "Index out of bounds"
      get (index, this)

      member this.Length: int =
        let rec length(l: IntList) =
          match l with
          | Empty -> 0
          | Cons(x, xs) -> 1 + length xs
        length this

    static member chop(i: int, l: IntList): int =
      let rec chop (key: int, list: IntList, min: int, max: int) =
        if(max < min) then
          -1
        else
          let mid = ((max - min) / 2) + min
          let key_at_mid = list.Get(mid)
          if key_at_mid < key then 
            chop(key, list, mid + 1, max)
          elif key_at_mid > key then 
            chop(key, list, min, mid - 1)
          else 
            mid
      chop(i, l, 0, l.Length - 1)
      