module MiniC.Core.Parsers

open FSharpPlus
open FParsec
open MiniC.Core.AST
open MiniC.Core.Combinators

let typeLexemCombinator =
    choice typeLexems |>> LexemParseResult.TypeLexem

let keywordLexemCombinator =
    choice (keywordLexems @ delimiterLexems) |>> LexemParseResult.Keyword

let operatorLexemCombinator =
    choice operatorLexems |>> LexemParseResult.BinaryOperator

let literalResultFunc (r: Result<Literal, 'b>) : Result<LexemParseResult, 'b> = r |> Result.map LexemParseResult.Literal

let identifierCombinator =
    identifierStringParser |>> LexemParseResult.Identifier

let literalLexemCombinator =
    choice [ attempt booleanLiteral
             attempt integerLiteral
             floatLiteral ]
    |>> literalResultFunc

let lexemParser =
    sepEndBy1
        (choice [ attempt typeLexemCombinator |>> Result.Ok
                  attempt keywordLexemCombinator |>> Result.Ok
                  attempt operatorLexemCombinator |>> Result.Ok
                  attempt literalLexemCombinator
                  identifierCombinator |>> Result.Ok ])
        ws
