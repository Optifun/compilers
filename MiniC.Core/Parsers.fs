﻿module MiniC.Core.Parsers

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
    identifierExpression |>> LexemParseResult.Identifier

let literalLexemCombinator =
    choice [ attempt booleanLiteral
             attempt integerLiteral
             floatLiteral ]
    |>> literalResultFunc
