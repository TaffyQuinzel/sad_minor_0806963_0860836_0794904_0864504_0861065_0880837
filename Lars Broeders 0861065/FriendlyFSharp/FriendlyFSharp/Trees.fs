module Trees
  type Vector3<[<Measure>] 'a> =
    {
      X: float<'a>
      Y: float<'a>
      Z: float<'a>
    }

    static member Zero: Vector3<'a> =
      { X = 0.0<_>; Y = 0.0<_>; Z = 0.0<_>}
    static member (+) (v1: Vector3<'a>, v2: Vector3<'a>): Vector3<'a> =
      { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
    static member (+) (v: Vector3<'a>, k: float<'a>): Vector3<'a> =
      { X = v.X + k; Y = v.Y + k; Z = v.Z + k }
    static member (+) (k:float<'a>, v: Vector3<'a>): Vector3<'a> =
      v + k
    static member (~-) (v: Vector3<'a>): Vector3<'a> =
      { X = -v.X; Y = -v.Y; Z = -v.Z }
    static member (-) (v1: Vector3<'a>, v2: Vector3<'a>): Vector3<'a> =
      v1 + (-v2)
    static member (-) (v: Vector3<'a>, k: float<'a>): Vector3<'a> =
      v + (-k)
    static member (-) (k: float<'a>, v: Vector3<'a>): Vector3<'a> =
      k + (-v)
    static member (*) (v1: Vector3<'a>, v2: Vector3<'b>): Vector3<'a * 'b> =
      { X = v1.X * v2.X; Y = v1.Y * v2.Y; Z = v1.Z * v2.Z }
    static member (*) (v: Vector3<'a> , f: float<'b>): Vector3<'a * 'b> =
      { X = v.X * f; Y = v.Y * f; Z = v.Z * f }
    static member (*) (f: float<'b>, v: Vector3<'a>): Vector3<'b * 'a> =
      { X = f * v.X; Y = f * v.Y; Z = f * v.Z }
    static member (/) (v: Vector3<'a>, f: float<'b>): Vector3<'a / 'b> =
      v * (1.0 / f)
    member this.Length: float<'a> =
      sqrt(this.X * this.X + this.Y * this.Y + this.Z * this.Z)
    member this.Normalized = 
      this / this.Length
    static member Distance (v1: Vector3<'a>, v2: Vector3<'a>): float<'a> =
      (v1 - v2).Length
    static member Normalize(v: Vector3<'a>): Vector3<1> =
      v.Normalized
    static member Dot (v1: Vector3<'a>, v2: Vector3<'b>): float<'a * 'b> =
      v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

  type Range<[<Measure>] 'u> =
    {
      Min: Vector3<'u>
      Size: Vector3<'u>
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

  type OctTree<'a, 'b, [<Measure>] 'u> =
    | Leaf of Range<'u> * List<'a> * Option<'b>
    | Node of Range<'u>           * OctTree<'a, 'b, 'u> * 
              OctTree<'a, 'b, 'u> * OctTree<'a, 'b, 'u> *
              OctTree<'a, 'b, 'u> * OctTree<'a, 'b, 'u> * 
              OctTree<'a, 'b, 'u> * OctTree<'a, 'b, 'u> * 
              OctTree<'a, 'b, 'u> * Option<'b>
    member this.Range =
      match this with
      | Leaf(r,_,_) -> r
      | Node(r,_,_,_,_,_,_,_,_,_) -> r

    member this.State =
      match this with
      | Leaf(_,_,Some s) -> s
      | Node(_,_,_,_,_,_,_,_,_,Some s) -> s
      | _ -> failwith "Null state"

    member this.ToList =
      match this with
      | Leaf(_,l,_) -> l
      | Node(_,a,b,c,d,e,f,g,h,_) -> a.ToList @ b.ToList @ c.ToList @ d.ToList @ e.ToList @ f.ToList @ g.ToList @ h.ToList

    static member mk_empty (min_range: Range<_>) (range: Range<_>) =
      let rec mk_empty (min: Range<_>) (r: Range<_>) =
        if min.Size.X < r.Size.X || min.Size.Y < r.Size.Y then
          let size' = r.Size / 2.0
          let r111 = { Min = r.Min; Size = size' }
          let r112 = { Min = r.Min + { size' with Y = 0.0<_>; Z = 0.0<_> }; Size = size' }
          let r121 = { Min = r.Min + { size' with X = 0.0<_>; Z = 0.0<_> }; Size = size' }
          let r122 = { Min = r.Min + { size' with Z = 0.0<_>}; Size = size' }
          let r211 = { Min = r.Min + { size' with X = 0.0<_>; Y = 0.0<_> }; Size = size' }
          let r212 = { Min = r.Min + { size' with Y = 0.0<_> }; Size = size' }
          let r221 = { Min = r.Min + { size' with X = 0.0<_> }; Size = size' }
          let r222 = { Min = r.Min + size'; Size = size' }
          Node(r,
            mk_empty min r111,
            mk_empty min r112,
            mk_empty min r121,
            mk_empty min r122,
            mk_empty min r211,
            mk_empty min r212,
            mk_empty min r221,
            mk_empty min r222,
            None
          )
        else
          Leaf(r, [], None)
      mk_empty min_range range
    
    static member insert position a =
      let rec insert position a =
        function
        | Leaf(range, l, s) -> Leaf(range, a :: l, s)
        | Node(r, n111, n112, n121, n122, n211, n212, n221, n222, s) ->
          let n111', n112', n121', n122', n211', n212', n221', n222' =
            if n111.Range.IsIn (position a) then
              (insert position a n111), n112, n121, n122, n211, n212, n221, n222
            elif n112.Range.IsIn (position a) then
              n111, (insert position a n112), n121, n122, n211, n212, n221, n222
            elif n121.Range.IsIn (position a) then
              n111, n112, (insert position a n121), n122, n211, n212, n221, n222
            elif n122.Range.IsIn (position a) then
              n111, n112, n121, (insert position a n122), n211, n212, n221, n222
            elif n211.Range.IsIn (position a) then
              n111, n112, n121, n122, (insert position a n211), n212, n221, n222
            elif n212.Range.IsIn (position a) then
              n111, n112, n121, n122, n211, (insert position a n212), n221, n222
            elif n221.Range.IsIn (position a) then
              n111, n112, n121, n122, n211, n212, (insert position a n221), n222
            else
              n111, n112, n121, n122, n211, n212, n221, (insert position a n222)
          Node(r, n111', n112', n121', n122', n211', n212', n221', n222', s)
      insert position a

    static member fold (f: 'b -> 'b -> 'b -> 'b -> 'b -> 'b -> 'b -> 'b -> 'b) (z: 'a list -> 'b) =
      let rec fold f z =
        function
        | Leaf(range, l, _) -> Leaf(range, l, Some(z l))
        | Node(range, n111, n112, n121, n122, n211, n212, n221, n222, _) ->
          let n111, n112, n121, n122, n211, n212, n221, n222 =
            fold f z n111,
            fold f z n112,
            fold f z n121,
            fold f z n122,
            fold f z n211,
            fold f z n212,
            fold f z n221,
            fold f z n222
          Node(range, n111, n112, n121, n122, n211, n212, n221, n222, Some(f n111.State n112.State n121.State n122.State n211.State n212.State n221.State n222.State))
      fold f z