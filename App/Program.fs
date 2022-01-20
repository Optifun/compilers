open System
open System.IO
open Shared.Expressions
open FParsec

let printLexems text =
    parseLexems text
    |> function
        | Result.Ok x -> x |> List.iter (printf "%s ")
        | Result.Error e -> printfn "Error %O" e

let printProgram program = program.Statements |> List.iter (printf "%O ")

let printAST text =
    parseCode text
    |> function
        | Result.Ok program -> printProgram program
        | Result.Error e -> printfn "Error %O" e


let test parser expression = 
    run (many1 parser) expression
    |> function
        | Success (r, _, _) -> List.iter (printf "%O ") r
        | Failure (e, _, _) -> printfn "%O" e

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

//    test expressionParser "abc > 23"
//    test ifThenElse "if abc > 23 then ... else if a=2 then ... ;;"

    while not breakLoop do
        [ ""; "[0] Exit"; "[1] Print Lexems"; "[2] Print AST" ] |> List.iter (printfn "%s")

        Console.ReadLine()
        |> function
            | "0" -> breakLoop <- true
            | "1" -> printLexems sourceCode
            | "2" -> printAST sourceCode
            | _ -> breakLoop <- false

    0
