module MiniC.Core.Tests.ASMTranslation

open MiniC.Core.AST
open NUnit.Framework
open FsUnit
open FParsec
open FSharpPlus
open FSharpPlus.Data
open MiniC.Core
open MiniC.Core.Analyzers
open MiniC.Core.Combinators.Syntax
open MiniC.Core.ASMTokens
open MiniC.Core.ASMTranslator


let runParser input parser = runParserOnString parser "" "some" input

let toResult<'a, 'b> (value: ParserResult<'a, 'b>) : Result<_, _> =
    value
    |> function
        | ParserResult.Success (v, _, _) -> Result.Ok v
        | ParserResult.Failure (x, _, _) -> Result.Error x

let translate (input: string) : Result<AToken list * VariableDecl Set, string> =
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
let ``Initialize variable with literal`` () =
    let input =
        "
        int a = 12;
        "

    let tokens: AToken list =
        [ Mov(ValueHolder.Variable "a", Literal <| HexInt 12) ]

    let variables =
        [ VariableDecl("a", WORD) ] |> Set.ofList

    translate input
    |> function
        | Result.Error msg -> failwith msg
        | Result.Ok (code, vars) ->
            TestContext.Out.WriteLine(printTokens code)
            code |> should equal tokens
            vars |> should equal variables


[<Test>]
let ``Initialize variable with variable`` () =
    let input =
        "
        int a = 12;
        int b = a;
        "

    let tokens: AToken list =
        [ Mov(ValueHolder.Variable "a", Literal <| HexInt 12)
          Mov(ValueHolder.Register AX, Variable "a")
          Mov(ValueHolder.Variable "b", Register AX) ]

    let variables =
        [ VariableDecl("a", WORD)
          VariableDecl("b", WORD) ]
        |> Set.ofList

    translate input
    |> function
        | Result.Error msg -> failwith msg
        | Result.Ok (code, vars) ->
            TestContext.Out.WriteLine(printTokens code)
            code |> should equal tokens
            vars |> should equal variables



[<Test>]
let ``Calling function`` () =
    let input =
        "
        int func(int a) {return 42;}
        int b = func(2);
        "

    let funcBody =
        [ Return(StackOperand.Literal(HexInt 42)) ]

    let funcParams = [ "a", asmType TypeLiteral.IntL ]

    let tokens: AToken list =
        [ Function("func", funcParams, funcBody) ]
        @ saveRegisters
          @ pushLiteral (HexInt 2)
            @ [ Call "func" ]
              @ popIdentifier "func" DX
                @ loadRegisters @ pushIdentifier "func" DX @ popIdentifier "b" DX

    let variables =
        [ VariableDecl("b", WORD)
          VariableDecl("a", WORD)
          VariableDecl("func", WORD) ]
        |> Set.ofList



    translate input
    |> function
        | Result.Error msg -> failwith msg
        | Result.Ok (code, vars) ->
            TestContext.Out.WriteLine(printTokens code)
            code |> should equal tokens
            vars |> should equal variables
