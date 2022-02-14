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
        [ TypeL.IntL
          TypeL.VoidL
          TypeL.BoolL
          TypeL.FloatL ]
        |> List.map TypeLexem

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal expected)


[<Test>]
let ``Keyword lexem parser parsers all keyword literals`` () =
    let input = "if else for while break return "
    let parser = sepEndBy1 keywordLexemCombinator <| ws

    let expected =
        [ Keyword.IF
          Keyword.ELSE
          Keyword.FOR
          Keyword.WHILE
          Keyword.BREAK
          Keyword.RETURN ]
        |> List.map Keyword

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal expected)

[<Test>]
let ``Keyword lexem parser parsers all delimiters`` () =
    let input = ";.({[]})"
    let parser = sepEndBy1 keywordLexemCombinator <| ws

    let expected =
        [ Keyword.Delimiter ";"
          Keyword.Delimiter "."
          Keyword.Delimiter "("
          Keyword.Delimiter "{"
          Keyword.Delimiter "["
          Keyword.Delimiter "]"
          Keyword.Delimiter "}"
          Keyword.Delimiter ")" ]
        |> List.map Keyword

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal expected)

[<Test>]
let ``Operator lexem parser parsers all operators`` () =
    let input = "== != <= < >= > || && + - * / %"
    let parser = sepEndBy1 operatorLexemCombinator <| ws

    let expected =
        [ Equal
          NotEqual
          LessEqual
          Less
          GreaterEqual
          Greater
          ConditionalOr
          ConditionalAnd
          Add
          Subtract
          Multiply
          Divide
          Modulus ]
        |> List.map LexemParseResult.BinaryOperator

    runParser input parser
    |> mapParserResult (fun v -> v |> should equal expected)
