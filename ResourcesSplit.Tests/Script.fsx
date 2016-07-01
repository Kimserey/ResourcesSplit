type Resource = 
    { Level: Level
      Continent: Continent }

and Level =
    | Good
    | Normal
    | Poor

and Continent =
    | Asia
    | Europe
    | Pacific

type Groups = {
    A: Resource list
    B: Resource list
    C: Resource list
} with
    static member Empty = 
        { A = []; B = []; C = [] }

    static member Place (resource: Resource) (x: Groups) =
        let sumLevel =
            List.sumBy (fun r -> match r.Level with Good -> 3 | Normal -> 2 | Poor -> 1)
        
        let countContinent =
            List.filter (fun r -> r.Continent = resource.Continent) >> List.length


        // Place a resource where there is the less resource of the same Level.
        // When groups have equal number of resources and decision cannot be made,
        // Place a resource where there is the less resource of the same Continent

        let selectGroup =
            [(x.A, "A"); (x.B, "B"); (x.C, "C") ]
            |> List.groupBy (fst >> sumLevel)
            |> List.minBy fst
            |> snd
            |> List.minBy (fst >> countContinent)
            |> snd

        match selectGroup with
        | "A" -> { x with A = resource :: x.A }
        | "B" -> { x with B = resource :: x.B }
        | _   -> { x with C = resource :: x.C }
     
let resources =
    [ { Level = Good; Continent = Asia }
      { Level = Good; Continent = Asia }
      { Level = Good; Continent = Pacific }
      { Level = Good; Continent = Pacific }
      { Level = Normal; Continent = Asia }
      { Level = Normal; Continent = Pacific }
      { Level = Poor; Continent = Asia }
      { Level = Poor; Continent = Asia } ]

(Groups.Empty, resources)
||> List.fold (fun groups resource -> Groups.Place resource groups)