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

type Statement =
    | IfThen of Expression * Statement
    | IfThenElse of Expression * Statement * Statement
    | Assertion of Expression * Expression
    | Empty

type Program = { Statements: Statement list }



let ws = skipMany (skipChar ' ')
let ws1 = skipMany1 (skipChar ' ')

let lexem str = pstring str

let lexemWs str = lexem str .>> ws1

let keywords = [ "if"; "then"; "else"; "true"; "false" ] |> List.map lexemWs

let other = [ "..."; ";"; "="; ">"; ">="; "<"; "<="; ":=" ] |> List.map lexem

let charThenString chr str = pipe2 (chr |>> Char.ToString) str (fun d rest -> d + rest)

let hexLexem = charThenString digit (manyChars hex)

let identifierLexem = charThenString letter (manyChars (letter <|> digit))

let lexems = hexLexem :: identifierLexem :: keywords @ other

let lexemParser = sepEndBy1 (choice lexems) ws

let parseLexems input =
    match run lexemParser input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err


let hexExpression = hexLexem <?> "Hex value" |>> Expression.HexNumber

let identifierExpression = identifierLexem <?> "Identifier" |>> Expression.Identifier

let boolExpression =
    pstring "true" <|> pstring "false" <?> "Boolean value"
    |>> function
        | "true" -> Expression.Boolean true
        | _ -> Expression.Boolean false



let opp = OperatorPrecedenceParser<Expression, _, _>()

opp.TermParser <-
    choice [ hexExpression
             identifierExpression ]

opp.AddOperator <| InfixOperator("=", ws, 2, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.Equals)))

opp.AddOperator <| InfixOperator(">", ws, 3, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThan)))

opp.AddOperator <| InfixOperator(">=", ws, 4, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThanOrEquals)))

opp.AddOperator <| InfixOperator("<", ws, 5, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThan)))

opp.AddOperator <| InfixOperator("<=", ws, 6, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThanOrEquals)))


let expressionParser =
    choice [ opp.ExpressionParser
             hexExpression
             identifierExpression
             boolExpression ]

let stmValue, stmRef = createParserForwardedToRef<Statement, unit> ()

let ifParser = skipString "if" >>. ws1 >>. expressionParser <?> "If"

let thenParser = ws >>. skipString "then" >>. ws1 >>. stmValue <?> "Then"

let elseParser = ws >>. skipString "else" >>. ws1 >>. stmValue <?> "Else"

let empty = skipString "..." >>% Statement.Empty <?> "Empty statement"

let assertion = identifierExpression .>> ws .>> skipString ":=" .>> ws .>>. expressionParser |>> Statement.Assertion <?> "Assertion"

let ifThen = ifParser .>>. thenParser |>> Statement.IfThen .>> ws .>> skipChar ';' <?> "If ... then ... statement"

let ifThenElse =
    pipe3 ifParser thenParser elseParser (fun ex st1 st2 -> Statement.IfThenElse(ex, st1, st2)) .>> ws .>> skipChar ';' <?> "If ... then ... else ... statement"

do
    stmRef
    := choice [ empty
                attempt assertion
                attempt ifThenElse
                attempt ifThen ]

let syntaxParser = sepEndBy1 stmValue ws

let parseCode input =
    let parserResult = run syntaxParser input

    match parserResult with
    | Success (res, _, _) -> Result.Ok { Statements = res }
    | Failure (err, _, _) -> Result.Error err
