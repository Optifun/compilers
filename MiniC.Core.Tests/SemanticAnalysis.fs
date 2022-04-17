module MiniC.Core.Tests.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open MiniC.Core.Error
open NUnit.Framework
open MiniC.Core.Analyzers
open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.AST
open NUnit.Framework
open FParsec
open FsUnit
open MiniC.Core.Combinators.Syntax


let runParser input parser = runParserOnString parser "" "some" input

let toImmutable (d: IDictionary<'a, 'c>) : ImmutableDictionary<'a, 'c> = d.ToImmutableDictionary()

let toResult<'a, 'b> (value: ParserResult<'a, 'b>) : Result<_, _> =
    value
    |> function
        | Success (v, _, _) -> Result.Ok v
        | Failure (x, _, _) -> Result.Error x


let constructContext (variables: Variable list) (functions: Function list) : Context =
    let vars =
        variables |> Seq.map (fun v -> v.Name, v) |> dict |> toImmutable

    let funcs =
        functions
        |> Seq.map (fun (fd, fb) -> fd.Name, (fd, fb))
        |> dict
        |> toImmutable

    { Variables = vars; Functions = funcs }

[<Test>]
let ``Initialisation type mismatch`` () =
    let input =
        "
        int abc = false;
        "

    let _statements =
        [ Initialization(varD ("abc", IntL), boolLiteral false) ]

    let _scope = constructContext [] [] |> Global

    let _errors: SemanticError list =
        [ TypeMismatch({ Name = "abc"; TypeDecl = IntL }, boolLiteral false) ]

    let (statements, scope, errors) =
        runParser input programParser
        |> toResult
        |> Result.map programAnalyzer
        |> Result.get

    statements |> should equal _statements
    errors |> should equal _errors
    scope |> should equal _scope
