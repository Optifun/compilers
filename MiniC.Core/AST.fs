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
    member x.GetTypeLiteral () =
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
    override x.ToString () =
        match x with
        | IntL -> "int"
        | FloatL -> "float"
        | BoolL -> "bool"
        | VoidL -> "void"
        | ArrayL (t, n) -> $"{t}[{n}]"

type Identifier = string

and Scope =
    | Global
    | Function of FunctionDecl

and FunctionDecl =
    { Parameters: Parameter list
      ReturnType: TypeLiteral
      Name: Identifier }


and Parameter =
    { TypeDecl: TypeLiteral
      Name: Identifier
    //      Function: Function
     }

and Variable =
    { TypeDecl: TypeLiteral
      Name: Identifier
    //      Scope: Scope
     }

let varD (name, typeL) : Variable = { TypeDecl = typeL; Name = name }

let paramD (name, typeL) : Parameter = { TypeDecl = typeL; Name = name }

let funcD (name, typeL, pars) : FunctionDecl =
    { Name = name
      ReturnType = typeL
      Parameters = pars }


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
    | ParamDeclaration of Parameter

and FunctionCall =
    { FuncName: Identifier
      Arguments: Expression list }

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

let intLiteral value : Expression = Literal << IntNumber <| value
let floatLiteral value : Expression = Literal << FloatNumber <| value
let boolLiteral value : Expression = Literal << Boolean <| value

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
