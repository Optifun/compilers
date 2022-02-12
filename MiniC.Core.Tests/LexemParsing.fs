module MiniC.Core.Tests

open MiniC.Core.AST
open NUnit.Framework
open FParsec
open FsUnit
open MiniC.Core.Parsers
open MiniC.Core.Combinators


let runParser input parser =
    runParserOnString parser "" "some" input

let mapParserResult<'a, 'b> (f: 'a -> unit) (value: ParserResult<'a, 'b>) : unit =
    value
    |> function
        | Success (v, _, _) -> f v
        | Failure (x, e, _) -> Assert.Fail(x.ToString())


[<Test>]
let ``Parse boolean literal`` () =
    let input = "bool"
    let parser = typeLexemParser TypeL.BoolL

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal TypeL.BoolL)


[<Test>]
let ``Type lexem parser parsers all types`` () =
    let input = "int void bool float"
    let parser = sepEndBy1 typeLexemCombinator <| ws

    runParser input parser
    |> mapParserResult (fun v -> v |> should haveLength 4)
