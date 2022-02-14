module MiniC.Core.Parsers

open FParsec
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
