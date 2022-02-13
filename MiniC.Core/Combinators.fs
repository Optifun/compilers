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
    [ ";"; "("; ")"; "{"; "}"; "["; "]" ]
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


let parseInteger (str: string) : Result<int, ErrorMessage> =
    let mutable value = -1

    if (Int32.TryParse(str, &value)) then
        Result.Ok value
    else
        Result.Error(ErrorMessage.InvalidIntegerLiteral str)


let parseFloat (str: string) : Result<float, ErrorMessage> =
    let mutable value = -1.0

    if (Double.TryParse(str, &value)) then
        Result.Ok value
    else
        Result.Error(ErrorMessage.InvalidFloatLiteral str)

let charThenString (chr: Parser<char, unit>) (str: Parser<string, unit>) =
    pipe2 (chr |>> Char.ToString) str (fun d rest -> d + rest)

let integerLiteral =
    charThenString digit (manyChars hex) <??> "Integer"
    |>> (parseInteger >> Result.map Literal.IntNumber)

let identifierExpression =
    charThenString letter (manyChars (letter <|> digit)) <??> "Identifier"

type LexemParseResult =
    | Literal of Literal
    | Keyword of Keyword
    | TypeLexem of TypeL
    | BinaryOperator of BinaryOp
    | Identifier of string
