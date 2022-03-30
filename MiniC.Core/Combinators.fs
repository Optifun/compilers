module MiniC.Core.Combinators

open System
open FSharpPlus
open FParsec
open MiniC.Core.AST
open MiniC.Core.Error

let ws: Parser<unit, _> = skipMany (skipChar ' ')

let whitespaced =
    seq {
        ' '
        '\n'
        '\r'
        '\t'
    }

let wslr: Parser<unit, _> =
    skipMany (skipAnyOf whitespaced) <?> "Whitespace characters"

let ws1 = skipMany1 (skipChar ' ')

let wslr1: Parser<unit, obj> =
    skipMany1 (skipAnyOf whitespaced) <?> "Whitespace1 characters"

let l str = pstring str

let lws str = l str .>> ws1


let genericParser<'T when 'T :> obj> (o: 'T) = l (o.ToString()) >>% o
let genericParserWs<'T when 'T :> obj> (o: 'T) = lws (o.ToString()) >>% o

let parseInteger (sign: char option, str: string) : Result<int, ErrorMessage> =
    let mutable value = -1

    match Int32.TryParse(str, &value), sign with
    | true, Some '-' -> Result.Ok -value
    | true, _ -> Result.Ok value
    | false, _ -> Result.Error <| ErrorMessage.InvalidIntegerLiteral str


let parseFloat (sign: char option, fstring: string) : Result<float, ErrorMessage> =
    let parseFloat = Result.protect float

    match parseFloat fstring, sign with
    | Result.Ok value, Some '-' -> Result.Ok -value
    | Result.Ok value, _ -> Result.Ok value
    | Result.Error _, _ -> Result.Error <| ErrorMessage.InvalidFloatLiteral fstring


let charThenString (chr: Parser<char, _>) (str: Parser<string, _>) =
    pipe2 (chr |>> Char.ToString) str (fun d rest -> d + rest)

let mapResultToReply msg res =
    res
    |> function
        | Result.Ok x -> preturn x
        | Result.Error e -> fail msg


[<AutoOpen>]
module Lexem =


    let typeLexemParser (t: TypeL) : Parser<TypeL, _> = genericParser t <?> "Type lexem"
    let operatorLexemParser (op: BinaryOp) : Parser<BinaryOp, _> = genericParser op <?> "Binary operator"


    let operatorLexems: Parser<BinaryOp, _> list =
        BinaryOp.GetCases() |> List.map operatorLexemParser

    let typeLexems: Parser<TypeL, _> list =
        TypeL.GetCases() |> List.map typeLexemParser

    let delimiterLexems: Parser<Keyword, string> list =
        [ ";"
          "."
          ","
          "("
          ")"
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

    let signOperation: Parser<char option, _> =
        opt (pchar '-' <|> pchar '+')

    let integerLiteral =
        signOperation .>>. many1Chars digit .>> notFollowedBy (pchar '.')
        <??> "Integer"
        |>> (parseInteger >> Result.map Literal.IntNumber)

    let floatLiteral =
        pipe3 signOperation (many1Chars digit .>> pchar '.') (manyChars digit) (fun sign d f -> sign, d + "." + f)
        <??> "Float"
        |>> (parseFloat >> Result.map Literal.FloatNumber)

    let booleanLiteral: Parser<Result<Literal, ErrorMessage>, string> =
        (pstring "true" <|> pstring "false") <??> "Boolean"
        |>> function
            | "true" -> Result.Ok <| Literal.Boolean true
            | "false" -> Result.Ok <| Literal.Boolean false
            | s -> Result.Error <| InvalidBooleanLiteral s

    let identifierStringParser: Parser<Identifier, string> =
        charThenString letter (manyChars (letter <|> digit)) <??> "Identifier"

    type LexemParseResult =
        | Literal of Literal
        | Keyword of Keyword
        | TypeLexem of TypeL
        | BinaryOperator of BinaryOp
        | Identifier of string

        static member bind (value: obj) =
            match value with
            | :? AST.Literal as v -> LexemParseResult.Literal v
            | :? AST.Keyword as v -> LexemParseResult.Keyword v
            | :? AST.TypeL as v -> LexemParseResult.TypeLexem v
            | :? AST.BinaryOp as v -> LexemParseResult.BinaryOperator v
            | :? string as v -> LexemParseResult.Identifier v
            | _ -> failwith "can't cast value to LexemParseResult"




module Syntax =
    let typeLexemParser =
        choice typeLexems |>> TypeLiteral.TypeL

    let operatorLexemCombinator = choice operatorLexems

    let literalLexemParser: Parser<Literal, _> =
        choice [ attempt booleanLiteral
                 attempt integerLiteral
                 floatLiteral ]
        >>= mapResultToReply "Incorrect literal"

    let argParser: Parser<Parameter, _> =
        typeLexemParser .>> ws1 .>>. identifierStringParser
        |>> fun (t, i) -> { Name = i; TypeDecl = t }

    let argsParser =
        sepBy1 argParser (ws >>. skipChar ',' >>. ws)

    let curlyP p = between (skipChar '(') (skipChar ')') p

    let funcDeclarationParser =
        typeLexemParser .>> ws1 .>>.? identifierStringParser .>> ws
        .>>. curlyP argsParser
        |>> fun ((t, i), p) ->
                { Name = i
                  ReturnType = t
                  Parameters = p }

    let expressionStub, expressionParserRef =
        createParserForwardedToRef ()

    let varDeclarationParser: Parser<Variable, string> =
        typeLexemParser .>> ws1 .>>.? identifierStringParser
        |>> fun (t, i) -> { Name = i; TypeDecl = t }

    let initializationParser: Parser<Initialization, string> =
        varDeclarationParser .>> ws .>> skipChar '=' .>> ws
        .>>. expressionStub

    let returnStm =
        skipString "return" >>. ws1 >>. expressionStub .>> skipChar ';'
        |>> Statement.Return

    //    let funcArgument =
//        (literalLexemParser |>> Argument.Literal)
//        <|> (identifierStringParser |>> Argument.Identifier)

    let funcCallParser =
        identifierStringParser .>>? ws
        .>>.? curlyP (sepBy expressionStub (ws >>. skipChar ',' >>. ws))
        |>> fun (f, args) -> { FuncName = f; Arguments = args }


    do
        expressionParserRef
        := choice [ attempt literalLexemParser |>> Expression.Literal
                    attempt funcCallParser |>> Expression.Call
                    identifierStringParser |>> Expression.Identifier ]

    let expressionParser =
        expressionParserRef.Value <?> "Expression"

    let statementStub, statementParserRef =
        createParserForwardedToRef ()

    let emptyStatementParser =
        skipChar ';' >>? many (skipChar ';') >>% Statement.Empty

    let blockParser: Parser<Block, _> =
        between (skipChar '{') (skipChar '}') (many1 statementStub)

    let functionParser: Parser<Function, _> =
        funcDeclarationParser .>>. between wslr wslr blockParser

    let simpleStatement =
        expressionParser .>> ws .>> skipChar ';' |>> Statement.Expression

    let simpleVar =
        varDeclarationParser .>> ws .>> skipChar ';'
        |>> Statement.VarDeclaration

    let simpleInit =
        initializationParser .>> ws .>> skipChar ';'
        |>> Statement.Initialization

    do
        statementParserRef
        := between wslr wslr
           <| choice [ attempt functionParser |>> Statement.FuncDeclaration
                       attempt blockParser |>> Statement.Block
                       attempt returnStm
                       attempt simpleInit
                       attempt simpleVar
                       attempt simpleStatement
                       emptyStatementParser ]

    let statementParser = statementParserRef.Value

    let programParser = many statementParser
