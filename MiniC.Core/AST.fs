module MiniC.Core.AST

open Microsoft.FSharp.Reflection

let internal GetCases<'Type> () =
    let cases = FSharpType.GetUnionCases(typeof<'Type>)

    [ for c in cases do
          yield (FSharpValue.MakeUnion(c, [||]) :?> 'Type) ]


type Literal =
    | IntNumber of Value: int
    | FloatNumber of Value: float
    | Boolean of Value: bool

    member x.GetTypeLiteral() =
        x
        |> function
            | IntNumber _ -> IntL
            | FloatNumber _ -> FloatL
            | _ -> BoolL

and TypeLiteral =
    | IntL
    | FloatL
    | BoolL
    | VoidL
    | ArrayL of TypeLiteral * int option

    override x.ToString() =
        match x with
        | IntL -> "int"
        | FloatL -> "float"
        | BoolL -> "bool"
        | VoidL -> "void"
        | ArrayL(t, n) -> $"{t}[{n}]"

type Identifier = string

and FunctionDecl = { Parameters: Variable list; ReturnType: TypeLiteral; Name: Identifier }


and Variable = { TypeDecl: TypeLiteral; Name: Identifier }

type Expression =
    | Assignment of Identifier * Expression
    | Binary of BinaryOp * Expression * Expression
    | Literal of Literal
    | Identifier of Identifier
    | Call of FunctionCall

and Statement =
    | Empty
    | Expression of Expression
    | Initialization of Initialization
    | Return of Expression
    | Block of Block
    | VarDeclaration of Variable
    | FuncDeclaration of Function
    | ParamDeclaration of Variable

and FunctionCall = { FuncName: Identifier; Arguments: Expression list }

and Initialization = Variable * Expression

and Block = Statement list

and Function = FunctionDecl * Block

and BinaryOp =
    | Equal
    | NotEqual
    | LessEqual
    | GreaterEqual
    | Assignment
    | Less
    | Greater
    | ConditionalOr
    | ConditionalAnd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus

    override x.ToString() =
        match x with
        | Assignment -> "="
        | ConditionalOr -> "||"
        | Equal -> "=="
        | NotEqual -> "!="
        | LessEqual -> "<="
        | Less -> "<"
        | GreaterEqual -> ">="
        | Greater -> ">"
        | ConditionalAnd -> "&&"
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"
        | Modulus -> "%"

    static member GetCases = GetCases<BinaryOp>

let varD (name, typeL) = { Name = name; TypeDecl = typeL }: Variable

let funcD (name, typeL, pars) : FunctionDecl =
    { Name = name; ReturnType = typeL; Parameters = pars }

let funcCall (name, args) = { FuncName = name; Arguments = args } |> Expression.Call

let intLiteral value = value |> IntNumber |> Literal
let floatLiteral value = value |> FloatNumber |> Literal
let boolLiteral value = value |> Boolean |> Literal

type Keyword =
    | IF
    | ELSE
    | WHILE
    | FOR
    | BREAK
    | RETURN
    | Delimiter of string

    override x.ToString() =
        match x with
        | IF -> "if"
        | WHILE -> "while"
        | ELSE -> "else"
        | FOR -> "for"
        | BREAK -> "break"
        | RETURN -> "return"
        | Delimiter d -> d
