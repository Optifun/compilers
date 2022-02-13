module MiniC.Core.Tests

open MiniC.Core.AST
open NUnit.Framework
open FParsec
open FsUnit
open MiniC.Core.Parsers
open MiniC.Core.Combinators


let runParser input parser = runParserOnString parser "" "some" input

let mapParserResult<'a, 'b> (f: 'a -> unit) (value: ParserResult<'a, 'b>) : unit =
    value
    |> function
        | Success (v, _, _) -> f v
        | Failure (x, _, _) -> Assert.Fail(x.ToString())


[<Test>]
let ``Parse 'bool' literal`` () =
    let input = "bool"
    let parser = typeLexemParser TypeL.BoolL

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal TypeL.BoolL)

[<Test>]
let ``Parse 'void' literal`` () =
    let input = "void"
    let parser = typeLexemParser TypeL.VoidL

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal TypeL.VoidL)

[<Test>]
let ``Parse 'int' literal`` () =
    let input = "int"
    let parser = typeLexemParser TypeL.IntL

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal TypeL.IntL)


[<Test>]
let ``Type lexem parser parsers all type literals`` () =
    let input = "int void bool float"
    let parser = sepEndBy1 typeLexemCombinator <| ws

    let expected =
        [TypeL.IntL
         TypeL.VoidL
         TypeL.BoolL
         TypeL.FloatL]
        |> List.map TypeLexem

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal expected)
