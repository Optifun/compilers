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


type LexemType =
    | Operator of string
    | Keyword of string
    | Expression of Expression

    member this.asExpression =
        this
        |> function
            | Expression ex -> Some ex
            | _ -> None
            
    member this.asKeyword =
        this
        |> function
            | Keyword ex -> Some ex
            | _ -> None

    member this.asOperator =
        this
        |> function
            | Operator ex -> Some ex
            | _ -> None
            
type Program = { Statements: Statement list }


let ws = skipMany (skipChar ' ')
let ws1 = skipMany1 (skipChar ' ')

let l str = pstring str

let lws str = l str .>> ws1

let keywords = [ "if"; "then"; "else" ] |> List.map (fun lexem -> lws lexem |>> LexemType.Keyword)

let other = [ ";"; "..." ] |> List.map (fun lexem -> l lexem |>> LexemType.Keyword)

let operators = [ "="; ">"; ">="; "<"; "<="; ":=" ] |> List.map (fun lexem -> l lexem |>> LexemType.Operator)

let charThenString chr str = pipe2 (chr |>> Char.ToString) str (fun d rest -> d + rest)

let hexExpression = charThenString digit (manyChars hex) <?> "Hex value" |>> Expression.HexNumber

let identifierExpression = charThenString letter (manyChars (letter <|> digit)) <?> "Identifier" |>> Expression.Identifier

let hexLexem = hexExpression |>> LexemType.Expression

let identifierLexem = identifierExpression |>> LexemType.Expression


let lexems = keywords @ operators @ other @ [ hexLexem; identifierLexem ]

let lexemParser = sepEndBy1 (choice lexems) ws

let parseLexems input =
    match run lexemParser input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err



let boolExpression =
    pstring "true" <|> pstring "false" <?> "Boolean value"
    |>> function
        | "true" -> Expression.Boolean true
        | _ -> Expression.Boolean false

let opp = OperatorPrecedenceParser<Expression, _, _>()

let whitespaced expr = between ws ws expr

opp.TermParser <-
    choice [ whitespaced hexExpression
             whitespaced identifierExpression ]

opp.AddOperator <| InfixOperator("=", ws, 2, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.Equals)))

opp.AddOperator <| InfixOperator(">", ws, 3, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThan)))

opp.AddOperator <| InfixOperator(">=", ws, 4, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.GreaterThanOrEquals)))

opp.AddOperator <| InfixOperator("<", ws, 5, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThan)))

opp.AddOperator <| InfixOperator("<=", ws, 6, Associativity.None, (fun x y -> Expression.Binary(x, y, BinaryExprType.LesserThanOrEquals)))

let operatorParser = opp.ExpressionParser <?> "Binary operator"

let expressionParser =
    operatorParser
    <|> (choice [ hexExpression
                  identifierExpression
                  boolExpression ])

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
