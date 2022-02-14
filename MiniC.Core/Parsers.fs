module MiniC.Core.Parsers

open FSharpPlus
open FParsec
open MiniC.Core.AST
open MiniC.Core.Combinators

let typeLexemCombinator =
    typeLexems
    |> List.map (fun l -> l |>> LexemParseResult.TypeLexem)
    |> choice

let keywordLexemCombinator =
    keywordLexems @ delimiterLexems
    |> List.map (fun l -> l |>> LexemParseResult.Keyword)
    |> choice

let operatorLexemCombinator =
    operatorLexems
    |> List.map (fun l -> l |>> LexemParseResult.BinaryOperator)
    |> choice

let literalResultFunc (r: Result<Literal, 'b>) : Result<LexemParseResult, 'b> = r |> Result.map LexemParseResult.Literal

let literalLexemCombinator =
    choice [ attempt booleanLiteral
             attempt integerLiteral
             floatLiteral ]
    |>> literalResultFunc
