module MiniC.Core.AST

open System.Linq.Expressions
open Microsoft.FSharp.Reflection

let internal GetCases<'Type> () =
    let cases = FSharpType.GetUnionCases(typeof<'Type>)

    [ for c in cases do
          yield (FSharpValue.MakeUnion(c, [||]) :?> 'Type) ]


type Literal =
    | IntNumber of Value: int
    | FloatNumber of Value: float
    | Boolean of Value: bool
    member x.GetTypeLiteral () =
        x
        |> function
            | IntNumber _ -> IntL |> TypeLiteral.TypeL
            | FloatNumber _ -> FloatL |> TypeLiteral.TypeL
            | _ -> BoolL |> TypeLiteral.TypeL

and TypeL =
    | IntL
    | FloatL
    | BoolL
    | VoidL
    override x.ToString () =
        match x with
        | IntL -> "int"
        | FloatL -> "float"
        | BoolL -> "bool"
        | VoidL -> "void"

    static member GetCases = GetCases<TypeL>

and TypeLiteral =
    | TypeL of TypeL
    | ArrayL of TypeL * int option

type Identifier =
    | Variable of Variable
    | Parameter of Parameter
    | Function of FunctionDecl

and Scope =
    | Global
    | Function of FunctionDecl

and FunctionDecl =
    { Parameters: Parameter list
      ReturnType: TypeLiteral
      Name: string }


and Parameter =
    { TypeDecl: TypeLiteral
      Name: string
    //      Function: Function
     }

and Variable =
    { TypeDecl: TypeLiteral
      Name: string
    //      Scope: Scope
     }

type Argument =
    | Identifier of string
    | Literal of Literal

type FunctionCall =
    { FuncName: string
      Arguments: Argument list }

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
    | Declaration of Identifier

and Initialization = Identifier * Literal

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
    override x.ToString () =
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

type Keyword =
    | IF
    | ELSE
    | WHILE
    | FOR
    | BREAK
    | RETURN
    | Delimiter of string
    override x.ToString () =
        match x with
        | IF -> "if"
        | WHILE -> "while"
        | ELSE -> "else"
        | FOR -> "for"
        | BREAK -> "break"
        | RETURN -> "return"
        | Delimiter d -> d
