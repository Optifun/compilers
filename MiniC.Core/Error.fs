module MiniC.Core.Error

type ErrorMessage =
    | InvalidIntegerLiteral of string
    | InvalidFloatLiteral of string
    | InvalidBooleanLiteral of string
