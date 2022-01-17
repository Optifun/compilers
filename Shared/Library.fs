﻿//Входной язык содержит операторы условия if … then … else и if … then, разделённые
//символом ; (точка с запятой). Операторы условия содержат идентификаторы, знаки
//сравнения <, >, =, шестнадцатеричные числа, знак присваивания (:=). Шестнадцатеричными
//числами считать последовательность цифр и символов a, b, c, d, e, f, начинающуюся с цифры
//(например, 89, 45ac, 0abc).
module Shared.Expressions

open System
open FParsec

type BinaryExprType =
    | Equals
    | GreaterThan
    | GreaterThanOrEquals
    | LesserThan
    | LesserThanOrEquals

type Expression =
    | HexNumber of string
    | Identifier of string
    | Boolean of Boolean
    | Binary of Expression * Expression * BinaryExprType

type Statement =
    | IfThen of Expression * Statement
    | IfThenElse of Expression * Statement * Statement
    | Empty
    | Assertion of Expression * Expression

type Program = { Statements: List<Statement> }



let ws = skipMany (skipChar ' ')
let ws1 = skipMany1 (skipChar ' ')

let emptyLexem = pstring "..."

let ifLexem = pstring "if" .>> ws1

let thenLexem = pstring "then" .>> ws1

let elseLexem = pstring "else" .>> ws1

let hexLexem = pipe2 (ws >>. digit |>> Char.ToString) (manyChars hex .>> ws) (fun d rest -> d + rest)

let identifierLexem = many1Chars (letter <|> digit) .>> ws

let boolLexem: Parser<string, unit> = pstring "true" <|> pstring "false"

let operLexem =
    choice [ pstring "="
             pstring ">"
             pstring ">="
             pstring "<"
             pstring "<="
             pstring ":=" ]

let lexemParser =
    sepEndBy1
        (choice [ ifLexem
                  thenLexem
                  elseLexem
                  hexLexem
                  identifierLexem
                  boolLexem
                  emptyLexem
                  operLexem ])
        ws


let parseLexems input =
    match run lexemParser input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err


let hexExpression = hexLexem |>> Expression.HexNumber

let identifierExpression = identifierLexem |>> Expression.Identifier


let boolExpression =
    boolLexem
    |>> function
        | "true" -> true
        | "false" -> false
        | entry -> failwithf "%s should be of type Boolean" entry
    |>> Expression.Boolean


let opp = OperatorPrecedenceParser<Expression, _, _>()

opp.TermParser <-
    choice [ hexExpression
             identifierExpression ]

opp.AddOperator <| InfixOperator("=", ws, 2, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.Equals)))

opp.AddOperator <| InfixOperator(">", ws, 3, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThan)))

opp.AddOperator <| InfixOperator(">=", ws, 4, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThanOrEquals)))

opp.AddOperator <| InfixOperator("<", ws, 5, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThan)))

opp.AddOperator <| InfixOperator("<=", ws, 6, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThanOrEquals)))

//opp.AddOperator <| InfixOperator(":=", ws, 8, Associativity.Left, (fun x y -> Expression.Assertion(x, y)))


let expressionParser =
    choice [ opp.ExpressionParser
             hexExpression
             identifierExpression
             boolExpression ]

let stmValue, stmRef = createParserForwardedToRef<Statement, unit> ()

let ifParser = ((ifLexem |>> ignore)  >>. expressionParser .>> ws1)

let thenParser = ((thenLexem |>> ignore) >>. stmValue)

let elseParser = ((elseLexem |>> ignore) >>. stmValue .>> ws)

let empty = ws >>. (emptyLexem |>> ignore) .>> ws >>% Statement.Empty

let assertion =
    pipe2 (ws >>. identifierExpression .>> ws .>> skipString ":=") (expressionParser .>> ws) (fun id ex -> Statement.Assertion(id, ex))

let ifThen = pipe2 ifParser thenParser (fun ex st -> Statement.IfThen(ex, st))

let ifThenElse = pipe3 ifParser thenParser elseParser (fun ex st1 st2 -> Statement.IfThenElse(ex, st1, st2))

do
    stmRef
    := choice [ empty
                ifThen
                ifThenElse
                assertion ]
