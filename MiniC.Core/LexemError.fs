namespace MiniC.Core.Error

type LexemError =
    | InvalidIntegerLiteral of string
    | InvalidFloatLiteral of string
    | InvalidBooleanLiteral of string
    | IncorrectLiteral of string
