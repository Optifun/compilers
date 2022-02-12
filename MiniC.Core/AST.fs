module MiniC.Core.AST


type Literal =
    | IntNumber of Value: int
    | FloatNumber of Value: float
    | Boolean of Value: bool

type TypeL =
    | IntL
    | FloatL
    | BoolL
    | VoidL

and ArrayL = TypeL * int option

and TypeLiteral =
    | TypeL
    | ArrayL


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
