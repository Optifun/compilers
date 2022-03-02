module MiniC.Core.Tests

open MiniC.Core.AST
open NUnit.Framework
open FSharpPlus
open FParsec
open FsUnit
open MiniC.Core.Parsers
open MiniC.Core.Combinators
open MiniC.Core.Combinators.Lexem


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
let ``Type lexem parser parses all type literals`` () =
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
let ``Keyword lexem parser parses all keyword literals`` () =
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
let ``Keyword lexem parser parses all delimiters`` () =
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
let ``Operator lexem parser parses all operators`` () =
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

[<Test>]
let ``Literal parser parses boolean literals`` () =
    let input = "true false false true"
    let parser = sepEndBy1 literalLexemCombinator <| ws

    let expected =
        [ Literal.Boolean true
          Literal.Boolean false
          Literal.Boolean false
          Literal.Boolean true ]
        |> List.map Literal

    runParser input parser
    |> mapParserResult (List.map Result.get >> fun v -> v |> should equal expected)

[<Test>]
let ``Literal parser parses integer literals`` () =
    let input = "1 3 -5 99999 -12345 67890"
    let parser = sepEndBy1 literalLexemCombinator <| ws

    let expected =
        [ Literal.IntNumber 1
          Literal.IntNumber 3
          Literal.IntNumber -5
          Literal.IntNumber 99999
          Literal.IntNumber -12345
          Literal.IntNumber 67890 ]
        |> List.map Literal

    runParser input parser
    |> mapParserResult (List.map Result.get >> fun v -> v |> should equal expected)


[<Test>]
let ``Literal parser parses float literals`` () =
    let input = "1.0 0. -6.6 0.12345 6789.0"
    let parser = sepEndBy1 literalLexemCombinator <| ws

    let expected =
        [ Literal.FloatNumber 1.
          Literal.FloatNumber 0.
          Literal.FloatNumber -6.6
          Literal.FloatNumber 0.12345
          Literal.FloatNumber 6789.0 ]
        |> List.map Literal

    runParser input parser
    |> mapParserResult (List.map Result.get >> fun v -> v |> should equal expected)

[<Test>]
let ``Lexem parser parses each lexem`` () =
    let input = "( a * 1.0 ) if true a < 2"

    let expectedList =
        [ LexemParseResult.bind <| Delimiter "("
          LexemParseResult.bind <| "a"
          LexemParseResult.bind <| BinaryOp.Multiply
          LexemParseResult.bind <| Literal.FloatNumber 1.0
          LexemParseResult.bind <| Delimiter ")"
          LexemParseResult.bind <| Keyword.IF
          LexemParseResult.bind <| Literal.Boolean true
          LexemParseResult.bind <| "a"
          LexemParseResult.bind <| BinaryOp.Less
          LexemParseResult.bind <| Literal.IntNumber 2 ]

    runParser input lexemParser
    |> mapParserResult (List.map Result.get >> fun l -> l |> should equal expectedList)
