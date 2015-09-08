module Chapter4
  module QuadTree =
    open Chapter2.Math
     
    type Range<[<Measure>] 'u> =
      {
        Min: Vector2<'u>
        Size: Vector2<'u>
      }

      member this.IsIn v =
        let d = v - this.Min
        v.X >= this.Min.X && v.Y >= this.Min.Y && d.X <= this.Size.X && d.Y <= this.Size.Y

    type QuadTree<'a, 'b, [<Measure>] 'u> =
      | Leaf of Range<'u> * List<'a> * Option<'b>
      | Node of Range<'u> * QuadTree<'a, 'b, 'u> * QuadTree<'a, 'b, 'u> * QuadTree<'a, 'b, 'u> * QuadTree<'a, 'b, 'u> * Option<'b>
       
      member this.Range =
        match this with
        | Leaf(r,_,_) -> r
        | Node(r,_,_,_,_,_) -> r

      member this.State =
        match this with
        | Leaf(_,_,Some s) -> s
        | Node(_,_,_,_,_,Some s) -> s
        | _ -> failwith "Null state"

      member this.ToList =
        match this with
        | Leaf(_,l,_) -> l
        | Node(_,a,b,c,d,_) -> a.ToList @ b.ToList @ c.ToList @ d.ToList

    let rec mk_empty (min_range: Range<_>) (range: Range<_>) =
      if min_range.Size.X < range.Size.X || min_range.Size.Y < range.Size.Y then
        let size' = range.Size / 2.0
        let range11 = { Min = range.Min; Size = size' }
        let range12 = { Min = range.Min + { size' with Y = 0.0<_> }; Size = size' }
        let range21 = { Min = range.Min + { size' with X = 0.0<_> }; Size = size' }
        let range22 = { Min = range.Min + size'; Size = size' }
        Node(range,
          mk_empty min_range range11,
          mk_empty min_range range12,
          mk_empty min_range range21,
          mk_empty min_range range22,
          None
        )
      else
        Leaf(range, [], None)

    let rec insert position a =
      function
      | Leaf(range, l, s) -> Leaf(range, a :: l, s)
      | Node(r, n11, n12, n21, n22, s) ->
        let n11', n12', n21', n22' =
          if n11.Range.IsIn (position a) then
            (insert position a n11), n12, n21, n22
          elif n12.Range.IsIn (position a) then
            n11, (insert position a n12), n21, n22
          elif n21.Range.IsIn (position a) then
            n11, n12, (insert position a n21), n22
          else
            n11, n12, n21, (insert position a n22)
        Node(r, n11', n12', n21', n22', s)

    let rec fold (f: 'b -> 'b -> 'b -> 'b -> 'b) (z: 'a list -> 'b) =
      function
      | Leaf(range, l, _) -> Leaf(range, l, Some(z l))
      | Node(range, n11, n12, n21, n22, _) ->
        let n11, n12, n21, n22 =
          fold f z n11,
          fold f z n12,
          fold f z n21,
          fold f z n22
        Node(range, n11, n12, n21, n22, Some(f n11.State n12.State n21.State n22.State))
  
  module LargeAsteroidFieldSimulation =
    open System
    open System.Threading
    open Chapter2.Math
    open Chapter3.SmallAsteroidFieldSimulation

    let f0 = create_field 200

    type Barycenter =
      {
        Position: Vector2<m>
        Mass: float<kg>
      }

      member this.ToAsteroid =
        {
          Position = this.Position
          Mass = this.Mass
          Name = ""
          Velocity = Vector2<_>.Zero
        }

      static member (+/) (b1: Barycenter, b2: Barycenter) =
        let new_mass = b1.Mass + b2.Mass
        if new_mass < 0.01<_> then
          {
            Position = Vector2<_>.Zero
            Mass = 0.0<_>
          }
        else
          {
            Position = (b1.Position * b1.Mass + b2.Position * b2.Mass) / new_mass
            Mass = new_mass
          }

      static member OfAsteroidList (l: Asteroid list) =
        let positions_weighted_sum = Seq.sum (Seq.map (fun (a: Asteroid) -> a.Position * a.Mass) l)
        let masses_sum = Seq.sumBy (fun (a: Asteroid) -> a.Mass) l
        {
          Position =
            if masses_sum > 0.01<_> then
              positions_weighted_sum / masses_sum
            else Vector2<_>.Zero
          Mass = masses_sum
        }

    let fast_simulation_step (asteroids: Asteroid list) =
      let empty_tree =
        QuadTree.mk_empty
          { Min = Vector2<_>.Zero; Size = { X = field_size / 8.0; Y = field_size / 8.0 } }
          { Min = Vector2<_>.Zero; Size = { X = field_size; Y = field_size } }
      let tree =
        List.fold (fun t a -> 
          QuadTree.insert (fun (a: Asteroid) -> a.Position) a t)
          empty_tree asteroids
      let tree = 
        QuadTree.fold (fun a b c d -> (a +/ b) +/ (c +/ d))
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
        | QuadTree.Leaf(r, a, b) -> QuadTree.Leaf(r, local_forces others a, b)
        | QuadTree.Node(r, q11, q12, q21, q22, b) ->
          let q11' =
            traverse (others +/ q12.State +/ q21.State +/ q22.State) q11
          let q12' =
            traverse (others +/ q11.State +/ q21.State +/ q22.State) q12
          let q21' =
            traverse (others +/ q11.State +/ q12.State +/ q22.State) q21
          let q22' =
            traverse (others +/ q11.State +/ q12.State +/ q21.State) q22
          QuadTree.Node(r, q11', q12', q21', q22', b)
      (traverse { Position = Vector2<_>.Zero; Mass = 0.0<_> } tree).ToList
    
    let s = Diagnostics.Stopwatch()
    let print_framerate (asteroids: Asteroid list) =
      do Console.Clear()
      let dt = s.Elapsed
      let dt = 1.0 / dt.TotalSeconds
      do Console.WriteLine(dt.ToString("0#.#"))

    let base_simulation print_scene simulation_step =
      let rec simulation m =
        do print_scene m
        do s.Reset()
        do s.Start()
        let m' = simulation_step m
        do s.Stop()
        do simulation m'
      do simulation f0

    let slow_simulation() =
      base_simulation print_scene simulation_step
    let slow_simulation_framerate() =
      base_simulation print_framerate simulation_step

    let fast_simulation() =
      base_simulation print_scene fast_simulation_step
    let fast_simulation_framerate() =
      base_simulation print_framerate fast_simulation_step
