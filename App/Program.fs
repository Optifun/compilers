open System
open System.IO
open Shared
open Shared
open Shared.Expressions
open FParsec

let printLexem =
    function
    | Expression ex -> printf "%O " ex
    | x -> printf "%O " x

let printLexems text =
    parseLexems text
    |> function
        | Result.Ok x -> x |> List.iter printLexem
        | Result.Error e -> printfn "Error %O" e

let asIdentifier (lexem: LexemType) =
    lexem.asExpression
    |> function
        | Some (Identifier i) -> Some i
        | _ -> None

let searchIdentifier (hashSet: string HashSet) (id: string) = hashSet.has <| id

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
            | Some path -> new StreamReader(path)
            | None -> Console.ReadLine() |> fun path -> new StreamReader(path)
        |> fun reader -> reader.ReadToEnd()

    let mutable breakLoop = false

    let hashSet = HashSet<string>(100)

    let search =
        searchIdentifier hashSet
        >> function
            | Some value -> printfn "%O" value
            | _ -> printfn "Not found"

    parseLexems sourceCode
    |> function
        | Result.Error e -> printfn "Error %O" e
        | Result.Ok x ->
            x
            |> List.map asIdentifier
            |> List.iter
                (function
                | Some x -> hashSet.add x
                | _ -> ())

    while not breakLoop do
        [ ""; "[0] Exit"; "[1] Print Lexems"; "[2] Print identifiers"; "[3] Search identifier"; "[4] Print AST" ] |> List.iter (printfn "%s")

        Console.ReadLine()
        |> function
            | "0" -> breakLoop <- true
            | "1" -> printLexems sourceCode
            | "2" -> hashSet.Print()
            | "3" -> search <| Console.ReadLine()
            | "4" -> printAST sourceCode
            | _ -> breakLoop <- false

    0
