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
            static member Default name = 
                { Name = name
                  Level = Good
                  Continent = Asia }

    and Level =
        | Good
        | Normal
        | Poor
        with
            override x.ToString() = sprintf "%A" x
            static member All = [ Good; Normal; Poor ]
            static member Color x = 
                match x with
                | Good -> "#95ea95"
                | Normal -> "#f9fb83"
                | Poor -> "#ffa8a8"

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
                List.sumBy (fun r -> match r.Level with Good -> 3 | Normal -> 2 | Poor -> 1)
        
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

[<JavaScript>]
module Client =
    open Domain
    
    let main =
        let resources = 
            ListModel.Create (fun r -> r.Name) 
                ([ Resource.Default "Tom"
                   Resource.Default "Lego"
                   Resource.Default "Sam"
                   Resource.Default "Tesla"
                   Resource.Default "Mono"
                   Resource.Default "Garcia"
                   Resource.Default "Lola"
                   Resource.Default "Tremon"
                   Resource.Default "Poka" ] |> List.sortBy (fun r -> r.Name))
    
        pre [ text "Place a resource where there is the less resources based on Level. When groups have equal number of resources and decision cannot be made, place a resource where there is the less resource of the same Continent" ]
        |> Doc.RunById "explanation"

        resources.View
        |> Doc.BindSeqCached (fun resource ->
            divAttr
                [ attr.style "margin: 15px 0" ]
                [ divAttr [ attr.style "width: 100px; display: inline-block; margin: 0 5px;" ] [ text resource.Name ]
                  Doc.Select [ attr.style "width: 100px; margin: 0 5px;" ] string Level.All (resources.LensInto (fun r -> r.Level) (fun r l -> { r with Level = l }) resource.Name)
                  Doc.Select [ attr.style "width: 100px; margin: 0 5px;" ] string Continent.All (resources.LensInto (fun r -> r.Continent) (fun r c -> { r with Continent = c }) resource.Name)
            ])
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
            
            let stats resources =
                dl [ dtAttr [ attr.``class`` "col-1" ] [ text "Good" ]
                     dd [ text (resources |> List.filter (fun r -> r.Level = Good) |> List.length |> string) ] 
                     dtAttr [ attr.``class`` "col-1 reset" ] [ text "Normal" ]
                     dd [ text (resources |> List.filter (fun r -> r.Level = Normal) |> List.length |> string) ] 
                     dt [ text "Poor" ]
                     dd [ text (resources |> List.filter (fun r -> r.Level = Poor) |> List.length |> string) ]
                     dt [ text "Asia" ]
                     dd [ text (resources |> List.filter (fun r -> r.Continent = Asia) |> List.length |> string) ]
                     dt [ text "Pacific" ]
                     dd [ text (resources |> List.filter (fun r -> r.Continent = Pacific) |> List.length |> string) ]
                     dt [ text "Europe" ] 
                     dd [ text (resources |> List.filter (fun r -> r.Continent = Europe) |> List.length |> string) ] ]

            let table =
                table 
                    [ yield tr [ th [ text "Group A" ]; th [ text "Group B" ]; th [ text "Group C" ] ] :> Doc
                      yield! (tableRow |> List.map(fun row -> tr (row |> List.map (function Some r -> tdAttr [ attr.style ("background-color:" + Level.Color r.Level) ] [ text r.Name; spanAttr [ attr.``class`` "small-text" ] [ text (sprintf "(%s)" (string r.Continent)) ] ] | None -> td []) |> Seq.cast)) |> Seq.cast) 
                      yield tr [ td [ stats groups.A ]; td [ stats groups.B ]; td [ stats groups.C ] ] :> Doc ]
        
            table)
        |> Doc.RunById "result"
