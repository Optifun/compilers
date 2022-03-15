module MiniC.Core.Tests.SyntaxParsing

open FSharpPlus
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
    |> (fun r -> r |> should equal expect)

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
    |> (fun r -> r |> should equal expect)

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
    |> (fun r -> r |> should equal expect)

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
    |> (fun r -> r |> should equal expect)
