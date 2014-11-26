open BowlingKata.SuperStronglyTyped

[<EntryPoint>]
let main argv = 
    
    let score = scoreGame "XXXXXXXXXXXX"
    printfn "%i" score

    System.Console.ReadKey() |> ignore

    0
