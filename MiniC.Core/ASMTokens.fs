module MiniC.Core.ASMTokens

type Identifier = string

type Register =
    | AX
    | BX
    | CX
    | DX
    | CI

    static member all () = [ AX; BX; CX; DX; CI ]

    override x.ToString () =
        match x with
        | AX -> "AX"
        | BX -> "BX"
        | CX -> "CX"
        | DX -> "DX"
        | CI -> "CI"

type BSize =
    | WORD
    | DWORD
    override x.ToString () =
        match x with
        | WORD -> "DH"
        | DWORD -> "DW"

type Variable = Identifier
type VariableDecl = Identifier * BSize

type Literal =
    | HexInt of int
    | HexFloat of double
    | Bool of bool

type StackOperand =
    | Register of Register
    | Literal of Literal

type ValueHolder =
    | Variable of Variable
    | Register of Register

type Value =
    | Register of Register
    | Variable of Variable
    | Literal of Literal

type AToken =
    | Push of StackOperand
    | Pop of Register
    | Mov of ValueHolder * Value
    | Call of Identifier
    | Label of Identifier
    | Function of Identifier * FunctionParams * FunctionBlock
    | Return of StackOperand

and FunctionParams = VariableDecl list
and FunctionBlock = AToken list
