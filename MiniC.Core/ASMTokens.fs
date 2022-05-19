module MiniC.Core.ASMTokens

type Identifier = string

type Register =
    | AX
    | BX
    | CX
    | DX
    | CI

    static member all () = [ AX; BX; CX; DX; CI ]

type Variable = Identifier

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
    | Call of Identifier
    | Push of StackOperand
    | Pop of Register
    | Mov of ValueHolder * Value
    | Label of Identifier
    | FunctionBlock of FunctionBlock
    | Return of StackOperand

and FunctionBlock = AToken list
