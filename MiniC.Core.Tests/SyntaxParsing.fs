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
    |> (fun v -> v |> should equal expect)
