open System
open System.IO
open Shared.Expressions

[<EntryPoint>]
let main argv =
    let filePath = argv |> Array.item 0
    let reader = new StreamReader(filePath)
    let text = reader.ReadToEnd()

    0
