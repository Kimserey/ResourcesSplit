namespace ResourcesSplit.Web

open WebSharper
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client
open WebSharper.JavaScript

[<JavaScript>]
module Domain =

    type Resource = 
        { Name: string
          Level: Level
          Continent: Continent }
        with 
            static member Green name = 
                { Name = name
                  Level = Green
                  Continent = Pacific }

            static member Yellow name = 
                { Name = name
                  Level = Yellow
                  Continent = Pacific }
                  
            static member Red name = 
                { Name = name
                  Level = Red
                  Continent = Pacific }

    and Level =
        | Green
        | Yellow
        | Red
        with
            override x.ToString() = sprintf "%A" x
            static member All = [ Green; Yellow; Red ]
            static member Color x = 
                match x with
                | Green -> "#95ea95"
                | Yellow -> "#f9fb83"
                | Red -> "#ffa8a8"

    and Continent =
        | Asia
        | Europe
        | Pacific
        with
            override x.ToString() = sprintf "%A" x
            static member All = [ Asia; Europe; Pacific ]

    type Groups = {
        A: Resource list
        B: Resource list
        C: Resource list
    } with
        static member Empty = 
            { A = []; B = []; C = [] }

        static member Place (resource: Resource) (x: Groups) =
            let sumLevel =
                List.sumBy (fun r -> match r.Level with Green -> 3 | Yellow -> 2 | Red -> 1)
        
            let countContinent =
                List.filter (fun r -> r.Continent = resource.Continent) >> List.length


            // Place a resource where there is the less resources based on Level.
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

    let initResources =
        [ Resource.Green "Jlaw"
          Resource.Green "Eyeball"
          Resource.Green "Ah fat"
          Resource.Green "Nkemp"
          Resource.Green "Roush"
          Resource.Green "Lunt"
          Resource.Green "Dys"
          Resource.Green "Grill"
          Resource.Green "Perfect"
          Resource.Green "Boots"
          Resource.Green "Slevin"
          Resource.Yellow "Ethan"
          Resource.Yellow "Bx"
          Resource.Yellow "Azeda"
          Resource.Yellow "Adodd"
          Resource.Yellow "Jawilki"
          Resource.Red "Che"
          Resource.Red "Felichque"
          Resource.Red "Wong"
          Resource.Red "Ultraduck"
          Resource.Red "Adiam"
          Resource.Red "Suplex"
          Resource.Red "Ugotowned"
          Resource.Red "Aisn"
          Resource.Red "Dwb"
          Resource.Red "Sirjokerloco"
          Resource.Red "Skulls"
          Resource.Red "Unknown"
          Resource.Red "Karin"
          Resource.Red "Jb163" ]

[<JavaScript>]
module Layout =
    open Domain 

    let stats resources =
        dl [ dt [ text "Green" ]; dd [ text (resources |> List.filter (fun r -> r.Level = Green) |> List.length |> string) ] 
             dt [ text "Yellow" ]; dd [ text (resources |> List.filter (fun r -> r.Level = Yellow) |> List.length |> string) ] 
             dt [ text "Red" ]; dd [ text (resources |> List.filter (fun r -> r.Level = Red) |> List.length |> string) ]
             dt [ text "Asia" ]; dd [ text (resources |> List.filter (fun r -> r.Continent = Asia) |> List.length |> string) ]
             dt [ text "Pacific" ]; dd [ text (resources |> List.filter (fun r -> r.Continent = Pacific) |> List.length |> string) ]
             dt [ text "Europe" ]; dd [ text (resources |> List.filter (fun r -> r.Continent = Europe) |> List.length |> string) ] ]


[<JavaScript>]
module Client =
    open WebSharper.UI.Next.Storage
    open Domain
    open System
    open Layout

    let resources =
        ListModel.CreateWithStorage (fun r-> r.Name) (LocalStorage "local-storage" (Serializer.Default<Resource>))

    let initResources() =
        initResources
        |> List.iter resources.Add

    let main =
        let newName = 
            Var.Create ""
            
        let reset() =
            newName.Value <- ""

        divAttr
            [ attr.``class`` "container-fluid"]
            [ resources.View
              |> Doc.BindSeqCached (fun resource ->
                divAttr
                    [ attr.``class`` "row my-1" ]
                    [ divAttr
                        [ attr.``class`` "col-4" ]
                        [ text resource.Name ]
                      divAttr
                        [ attr.``class`` "col-4" ]
                        [ Doc.Select [ attr.``class`` "form-control" ] string Level.All (resources.LensInto (fun r -> r.Level) (fun r l -> { r with Level = l }) resource.Name) ]
                      divAttr
                        [ attr.``class`` "col-4" ]
                        [ Doc.Select [ attr.``class`` "form-control" ] string Continent.All (resources.LensInto (fun r -> r.Continent) (fun r c -> { r with Continent = c }) resource.Name) ]
                    ])

              form
                [ divAttr
                    [ attr.``class`` "form-group my-3" ] 
                    [ Doc.Input 
                        [ attr.placeholder "Enter new resource name"
                          attr.``class`` "form-control"
                          attr.style "max-width: 300px" ] 
                        newName ]
                  divAttr 
                    [ attr.``class`` "mt-3" ]
                    [ Doc.Button "Add"
                        [ attr.``class`` "btn btn-primary mx-3 my-3"
                          attr.disabledDynPred (View.Const "true") (newName.View |> View.Map (fun v -> String.IsNullOrWhiteSpace v)) ] 
                        (fun () -> resources.Add (Resource.Green newName.Value); reset())

                      Doc.Button "Remove" 
                        [ attr.``class`` "btn btn-warning mx-3 my-3"
                          attr.disabledDynPred (View.Const "true") (newName.View |> View.Map (fun v -> String.IsNullOrWhiteSpace v)) ] 
                        (fun () -> resources.RemoveByKey newName.Value; reset()) 
                      
                      Doc.Button "Reset to original" 
                        [ attr.``class`` "btn btn-danger mx-3 my-3"] 
                        (fun () -> resources.Clear(); initResources()) ] ] ]
        |> Doc.RunById "resources"

        resources.View
        |> Doc.BindView (fun resources ->
            let groups =
                (Groups.Empty, resources |> Seq.toList)
                ||> List.fold (fun groups resource -> Groups.Place resource groups)
            
            let max =
                [ groups.A; groups.B; groups.C ] |> List.map List.length |> List.max

            let tableRow =
                [ for i in [0..max-1] do yield [ List.tryItem i groups.A; List.tryItem i groups.B; List.tryItem i groups.C ] ]
            
            let table =
                tableAttr
                    [ attr.``class`` "table table-striped table-bordered" ] 
                    [ yield tr [ th [ text "Group A" ]; th [ text "Group B" ]; th [ text "Group C" ] ] :> Doc
                      yield! (tableRow |> List.map(fun row -> tr (row |> List.map (function Some r -> tdAttr [ attr.style ("background-color:" + Level.Color r.Level) ] [ text r.Name; spanAttr [ attr.``class`` "small-text" ] [ text (sprintf "(%s)" (string r.Continent)) ] ] | None -> td []) |> Seq.cast)) |> Seq.cast) 
                      yield tr [ td [ stats groups.A ]; td [ stats groups.B ]; td [ stats groups.C ] ] :> Doc ]
        
            table)
        |> Doc.RunById "result"