module MiniC.Core.Tests.SyntaxParsing

open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.AST
open MiniC.Core.AST
open NUnit.Framework
open FParsec
open FsUnit
open MiniC.Core.Combinators.Syntax


let runParser input parser = runParserOnString parser "" "some" input

let toResult<'a, 'b> (value: ParserResult<'a, 'b>) : Result<_, _> =
    value
    |> function
        | Success (v, _, _) -> Result.Ok v
        | Failure (x, _, _) -> Result.Error x


[<Test>]
let ``Parse arguments definition`` () =
    let input = "int a, bool b, float c, int d"
    let parser = argsParser

    let expect: Parameter list =
        [ { Name = "a"
            TypeDecl = TypeLiteral.TypeL IntL }
          { Name = "b"
            TypeDecl = TypeLiteral.TypeL BoolL }
          { Name = "c"
            TypeDecl = TypeLiteral.TypeL FloatL }
          { Name = "d"
            TypeDecl = TypeLiteral.TypeL IntL } ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse one argument definition`` () =
    let input = "int a"
    let parser = argsParser

    let expect: Parameter list =
        [ { Name = "a"
            TypeDecl = TypeLiteral.TypeL IntL } ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse variable declaration`` () =
    let input = "int a;"
    let parser = simpleVar

    let expect: Statement =
        Statement.Declaration
        <| Variable
            { Name = "a"
              TypeDecl = TypeLiteral.TypeL IntL }

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse variable initialisation`` () =
    let input = "int a = 4;"
    let parser = simpleInit

    let expect: Statement =
        (Variable
            { Name = "a"
              TypeDecl = TypeLiteral.TypeL IntL },
         Literal.IntNumber 4)
        |> Statement.Initialization

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse int literal as expression`` () =
    let input = "4"
    let parser = expressionParser

    let expect: Expression =
        Literal.IntNumber 4 |> Expression.Literal

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse float literal as expression`` () =
    let input = "0.5"
    let parser = expressionParser

    let expect: Expression =
        Literal.FloatNumber 0.5 |> Expression.Literal

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with three args as expression`` () =
    let input = "func(a, 4, true);"
    let parser = expressionParser

    let expect: Expression =
        { FuncName = "func"
          Arguments =
            [ Argument.Identifier "a"
              Argument.Literal <| Literal.IntNumber 4
              Argument.Literal <| Literal.Boolean true ] }
        |> Expression.Call

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with two args without whitespaces as expression`` () =
    let input = "func(true,false);"
    let parser = expressionParser

    let expect: Expression =
        { FuncName = "func"
          Arguments =
            [ Argument.Literal <| Literal.Boolean true
              Argument.Literal <| Literal.Boolean false ] }
        |> Expression.Call

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with zero args as expression`` () =
    let input = "func();"
    let parser = expressionParser

    let expect =
        Expression.Call { FuncName = "func"; Arguments = [] }

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse statement block`` () =
    let input =
        "
        {
        ;
        ;
        ;
        }
        "

    let parser = statementParser

    let expect =
        Statement.Block [ Statement.Empty
                          Statement.Empty
                          Statement.Empty ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect
