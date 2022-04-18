namespace MiniC.Core.Error

open MiniC.Core.AST

type SemanticError =
    | TypeMismatch of Variable * Expression
    | ParameterTypeMismatch of Variable * Expression
    | ExpectedType of TypeLiteral * Expression
    | FunctionCallWrongParameters of FunctionDecl * FunctionCall
    | UnknownFunctionCall of Identifier
    | UnknownIdentifier of Identifier
    | IdentifierCollision of Identifier * string
    | VoidVariableType of Variable
