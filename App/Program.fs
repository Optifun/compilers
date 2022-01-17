open System
open System.IO
open Shared.Expressions

[<EntryPoint>]
let main argv =
    let filePath = argv |> Array.item 0
    let reader = new StreamReader(filePath)
    let text = reader.ReadToEnd()

    parseLexems text
    |> function
        | Result.Ok x -> x |> List.map (fun l -> printf "%s " l) |> ignore
        | Result.Error e -> printfn "Error %O" e

    0
