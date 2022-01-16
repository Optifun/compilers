//Входной язык содержит операторы условия if … then … else и if … then, разделённые
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
    | Assertion of Expression * Expression

type Statement =
    | IfThen of Expression * Statement
    | IfThenElse of Expression * Statement * Statement
    | Empty

type Program = { Statements: List<Statement> }

let ws = skipMany (skipChar ' ')
let ws1 = skipMany1 (skipChar ' ')

let hexLiteral = digit >>. manyChars hex |>> Expression.HexNumber .>> ws

let identifier = many1Chars (letter <|> digit) |>> Expression.Identifier .>> ws

let boolLiteral =
    pstring "true" <|> pstring "false"
    |>> function
        | "true" -> true
        | "false" -> false
        | entry -> failwithf "%s should be of type Boolean" entry
    |>> Expression.Boolean

let opp = OperatorPrecedenceParser<Expression, _, _>()

opp.TermParser <- choice [ hexLiteral; identifier ]

opp.AddOperator <| InfixOperator(":=", ws, 2, Associativity.Left, (fun x y -> Expression.Assertion(x, y)))

opp.AddOperator <| InfixOperator("=", ws, 2, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.Equals)))

opp.AddOperator <| InfixOperator(">", ws, 3, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThan)))

opp.AddOperator <| InfixOperator(">=", ws, 4, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThanOrEquals)))

opp.AddOperator <| InfixOperator("<", ws, 5, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThan)))

opp.AddOperator <| InfixOperator("<=", ws, 6, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThanOrEquals)))

let expressionParser = opp.ExpressionParser

let stmValue, stmRef = createParserForwardedToRef<Statement, unit> ()

let empty = ws >>. stringReturn "..." Statement.Empty .>> ws

let ifParser = (skipString "if" >>. ws1 >>. expressionParser .>> ws1)

let thenParser = (skipString "then" >>. ws1 >>. stmValue)

let elseParser = (skipString "else" >>. ws1 >>. stmValue .>> ws)

let ifThen = pipe2 ifParser thenParser (fun ex st -> Statement.IfThen(ex, st))

let ifThenElse = pipe3 ifParser thenParser elseParser (fun ex st1 st2 -> Statement.IfThenElse(ex, st1, st2))

do stmRef := choice [ empty; ifThen; ifThenElse ]

