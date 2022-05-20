module MiniC.Core.Tests.ASMTranslation

open NUnit.Framework
open FsUnit
open FParsec
open FSharpPlus
open FSharpPlus.Data
open MiniC.Core
open MiniC.Core.Analyzers
open MiniC.Core.Combinators.Syntax
open MiniC.Core.ASMTokens


let runParser input parser = runParserOnString parser "" "some" input

let toResult<'a, 'b> (value: ParserResult<'a, 'b>) : Result<_, _> =
    value
    |> function
        | ParserResult.Success (v, _, _) -> Result.Ok v
        | ParserResult.Failure (x, _, _) -> Result.Error x

let translate (input: string) : Result<AToken list * VariableDecl list, string> =
    monad' {
        let! programText = runParser input programParser |> toResult

        let statements, scope, errors =
            programAnalyzer programText

        if (errors.Length > 0) then
            return! Result.Error $"Errors = %A{errors}"
        else
            return! Result.Ok <| ASMTranslator.programTranslator statements scope
    }

[<Test>]
let ``Initialisation type mismatch`` () =
    let input =
        "
        int func(int a) {return 42;}
        int b = func(2);
        "

    let tokens: AToken list = []

    let variables = []

    translate input
    |> function
        | Result.Error msg -> failwith msg
        | Result.Ok (code, vars) ->
            code |> should equal tokens
            vars |> should equal variables
