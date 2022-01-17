open System
open System.IO
open Shared.Expressions

let printLexems text =
    parseLexems text
    |> function
        | Result.Ok x -> x |> List.map (fun l -> printf "%s " l) |> ignore
        | Result.Error e -> printfn "Error %O" e

[<EntryPoint>]
let main argv =
    let sourceCode =
        argv
        |> Array.tryItem 0
        |> function
            | Some filePath -> new StreamReader(filePath)
            | None -> failwith "Error: require path to a file as cmd argument"
        |> fun reader -> reader.ReadToEnd()

    let mutable breakLoop = false

    while not breakLoop do
        [ ""; "[0] Exit"; "[1] Print Lexems"; "[2] Print AST" ] |> List.map (printfn "%s") |> ignore

        Console.ReadLine()
        |> function
            | "0" -> breakLoop <- true
            | "1" -> printLexems sourceCode
            | _ -> breakLoop <- false

    0
