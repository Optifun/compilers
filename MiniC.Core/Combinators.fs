[<Sealed>]
module MiniC.Core.Combinators

open System
open FSharpPlus
open FParsec
open MiniC.Core.AST
open MiniC.Core.Error


let ws: Parser<unit, string> = skipMany (skipChar ' ')
let ws1 = skipMany1 (skipChar ' ')

let l str = pstring str

let lws str = l str .>> ws1


let genericParser<'T when 'T :> obj> (o: 'T) = l (o.ToString()) >>% o
let genericParserWs<'T when 'T :> obj> (o: 'T) = lws (o.ToString()) >>% o

let typeLexemParser (t: TypeL) : Parser<TypeL, string> = genericParser t <?> "Type lexem"
let operatorLexemParser (op: BinaryOp) : Parser<BinaryOp, string> = genericParser op <?> "Binary operator"


let operatorLexems: Parser<BinaryOp, string> list =
    BinaryOp.GetCases() |> List.map operatorLexemParser

let typeLexems: Parser<TypeL, string> list =
    TypeL.GetCases() |> List.map typeLexemParser

let delimiterLexems: Parser<Keyword, string> list =
    [ ";"
      "("
      ")"
      "."
      "{"
      "}"
      "["
      "]" ]
    |> List.map (fun lexem -> l lexem |>> Keyword.Delimiter)

let keywordLexems: Parser<Keyword, string> list =
    [ "if"
      "while"
      "else"
      "for"
      "break"
      "return" ]
    |> List.zip [ Keyword.IF
                  Keyword.WHILE
                  Keyword.ELSE
                  Keyword.FOR
                  Keyword.BREAK
                  Keyword.RETURN ]
    |> List.map (fun (key, str) -> lws str >>% key)


let parseInteger (sign: char option, str: string) : Result<int, ErrorMessage> =
    let mutable value = -1

    match Int32.TryParse(str, &value), sign with
    | true, Some '-' -> Result.Ok -value
    | true, _ -> Result.Ok value
    | false, _ -> Result.Error(ErrorMessage.InvalidIntegerLiteral str)


let parseFloat (sign: char option, (str: string, str2: string)) : Result<float, ErrorMessage> =
    let mutable value = -1.0
    let target = str + "." + str2

    match Double.TryParse(target, &value), sign with
    | true, Some '-' -> Result.Ok -value
    | true, _ -> Result.Ok value
    | false, _ -> Result.Error(ErrorMessage.InvalidFloatLiteral target)


let charThenString (chr: Parser<char, string>) (str: Parser<string, string>) =
    pipe2 (chr |>> Char.ToString) str (fun d rest -> d + rest)


let signOperation: Parser<char option, string> =
    opt (pchar '-' <|> pchar '+')

let integerLiteral =
    signOperation .>>. many1Chars digit .>> notFollowedBy (pchar '.')
    <??> "Integer"
    |>> (parseInteger >> Result.map Literal.IntNumber)

let floatLiteral =
    (signOperation
     .>>. (many1Chars digit .>> skipChar '.' .>>. manyChars digit))
    <??> "Float"
    |>> (parseFloat >> Result.map Literal.FloatNumber)

let booleanLiteral: Parser<Result<Literal, ErrorMessage>, string> =
    (pstring "true" <|> pstring "false") <??> "Boolean"
    |>> function
        | "true" -> Result.Ok <| Literal.Boolean true
        | "false" -> Result.Ok <| Literal.Boolean false
        | s -> Result.Error <| InvalidBooleanLiteral s

let identifierExpression =
    charThenString letter (manyChars (letter <|> digit)) <??> "Identifier"


type LexemParseResult =
    | Literal of Literal
    | Keyword of Keyword
    | TypeLexem of TypeL
    | BinaryOperator of BinaryOp
    | Identifier of string
