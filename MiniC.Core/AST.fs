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

type TypeL =
    | IntL
    | FloatL
    | BoolL
    | VoidL
    override x.ToString() =
        match x with
        | IntL -> "int"
        | FloatL -> "float"
        | BoolL -> "bool"
        | VoidL -> "void"

    static member GetCases = GetCases<TypeL>

and TypeLiteral =
    | TypeL
    | ArrayL of TypeL * int option

type Identifier =
    | Variable of Variable
    | Parameter of Parameter
    | Function of Function

and Scope =
    | Global
    | Function of Function

and Function =
    { Parameters: Parameter list
      ReturnType: TypeLiteral
      Name: string }

and Parameter =
    { TypeDecl: TypeLiteral
      Name: string
      Function: Function }

and Variable =
    { TypeDecl: TypeLiteral
      Name: string
      Scope: Scope }


type Expression =
    | Empty
    | Assignment of Identifier * Expression
    | Binary of BinaryOp * Expression * Expression
    | Literal of Literal
    | Identifier of Identifier

and BinaryOp =
    | ConditionalOr
    | Equal
    | NotEqual
    | LessEqual
    | Less
    | GreaterEqual
    | Greater
    | ConditionalAnd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    override x.ToString() =
        match x with
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
