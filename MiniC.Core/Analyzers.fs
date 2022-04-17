module MiniC.Core.Analyzers

open System
open System.Collections.Generic
open System.Collections.Immutable
open FSharpPlus
open FParsec
open FSharpPlus
open FSharpPlus.Control
open MiniC.Core.Combinators
open MiniC.Core.AST
open MiniC.Core.Error


let isOk result =
    match result with
    | Result.Ok _ -> true
    | _ -> false

type Scope =
    | Global of Context
    | Function of CompoundContext

and CompoundContext = Scope * Context

and Context =
    { Functions: ImmutableDictionary<Identifier, Function>
      Variables: ImmutableDictionary<Identifier, Variable> }

[<AutoOpen>]
module Context =
    let registerVariable context var =
        { context with Variables = context.Variables.Add <| (var.Name, var) }

    let registerFunction context (decl: FunctionDecl, block) =
        { context with Functions = context.Functions.Add <| (decl.Name, (decl, block)) }

    let getFunction context =
        Result.protect <| fun (id: Identifier) -> context.Functions.[id]

    let getVariable context =
        Result.protect <| fun (id: Identifier) -> context.Variables.[id]

    let hasFunction context (id: Identifier) = Seq.contains id context.Functions.Keys

    let hasVariable context (id: Identifier) = Seq.contains id context.Variables.Keys
    let hasIdentifier context (id: Identifier) = hasFunction context id || hasVariable context id

type Context with
    member x.registerVariable (var: Variable) = registerVariable x var
    member x.registerFunction (func: Function) = registerFunction x func
    member x.getFunction (id: Identifier) = getFunction x id
    member x.getVariable (id: Identifier) = getFunction x id
    member x.hasFunction (id: Identifier) = hasFunction x id
    member x.hasVariable (id: Identifier) = hasVariable x id
    member x.hasIdentifier (id: Identifier) = hasIdentifier x id


let rec visitScope (scope: Scope) (visitor: Context -> Identifier -> Result<'elem, 'TErr>) id : Result<'elem, 'TErr> =
    scope
    |> function
        | Global context -> visitor context id
        | Function (parent, current) ->
            visitor current id
            |> function
                | Result.Ok elem -> Result.Ok elem
                | Result.Error _ -> visitScope parent visitor id

type Scope with
    member x.getFunction (id: Identifier) = visitScope x getFunction id

    member x.getVariable (id: Identifier) = visitScope x getVariable id

    member x.registerVariable (var: Variable) =
        x
        |> function
            | Global c -> Global <| c.registerVariable var
            | Function (p, c) -> Function(p, c.registerVariable var)

    member x.registerFunction (func: Function) =
        x
        |> function
            | Global c -> Global <| c.registerFunction func
            | Function (p, c) -> Function(p, c.registerFunction func)


let funcRetType (func: Function) =
    let decl, body = func
    decl.ReturnType


let expressionAnalyzer (node: Expression) (context: Scope) : Result<Expression, SemanticError> =

    let rec visit expr =
        expr
        |> function
            | Literal literal -> Result.Ok expr
            | Call funcCall ->
                if (isOk <| context.getFunction funcCall.FuncName) then
                    Result.Ok expr
                else
                    Result.Error <| UnknownFunctionCall funcCall.FuncName

            | Identifier id ->
                if (isOk <| context.getVariable id) then
                    Result.Ok expr
                else
                    Result.Error <| UnknownIdentifier id
            | _ -> Result.Ok expr

    visit node


let matchTypeWith typeT (expr: Expression) (context: Scope) : Result<Expression, SemanticError> =
    match typeT, expr with
    | varType, Literal literal when varType = literal.GetTypeLiteral() -> Result.Ok expr
    | varType, Call funcCall when
        context.getFunction funcCall.FuncName
        |> Result.map (fun f -> varType = funcRetType f)
        |> Result.defaultValue false
        ->
        Result.Ok expr
    | varType, Identifier id when
        context.getVariable id
        |> Result.map (fun v -> varType = v.TypeDecl)
        |> Result.defaultValue false
        ->
        Result.Ok expr
    | _ -> Result.Error <| ExpectedType(typeT, expr)


let initializationAnalyzer (node: Initialization) (context: Scope) : Scope * Result<Initialization, SemanticError> =
    let var, value = node

    let newContext = context.registerVariable var

    let result =
        expressionAnalyzer value context
        |> Result.bind (fun exp -> matchTypeWith var.TypeDecl exp context)
        |> Result.bindError (fun _ -> Result.Error <| TypeMismatch(var, value))

    match result with
    | Result.Ok _ -> newContext, Result.Ok node
    | Result.Error error -> context, Result.Error error

let funcCallAnalyzer (node: FunctionCall) (context: Scope) : Scope * Result<FunctionCall, SemanticError> =

    let checkParamTypes (param: Parameter, arg: Expression) =
        let result =
            expressionAnalyzer arg context
            |> Result.bind (fun exp -> matchTypeWith param.TypeDecl exp context)
            |> Result.bindError (fun _ -> Result.Error <| ParameterTypeMismatch(param, arg))

        match result with
        | Result.Ok _ -> true
        | Result.Error error -> false


    let func = context.getFunction node.FuncName

    let succeed =
        func
        |> Result.map (fun (f, _) -> List.zip f.Parameters node.Arguments)
        |> (Result.map <| List.forall checkParamTypes)
        |> Result.defaultValue false


    let response =
        func
        |> Result.mapError (fun _ -> UnknownFunctionCall(node.FuncName))
        |> Result.bind (
            function
            | _ when succeed -> Result.Ok node
            | f, _ -> Result.Error <| FunctionCallWrongParameters(f, node)
        )

    context, response

let functionDeclAnalyzer (func: Function) (context: Scope) : Scope * Result<Function, SemanticError> =
    let decl, _ = func

    if (isOk <| context.getVariable decl.Name
        || isOk <| context.getFunction decl.Name) then
        context,
        Result.Error
        <| IdentifierCollision(decl.Name, "Identifier already in use")
    else
        context.registerFunction func, Result.Ok func


let getError =
    function
    | Result.Ok _ -> []
    | Result.Error e -> [ e ]

let programAnalyzer (statements: Statement list) =

    let analyze (statement: Statement) (context: Scope) : Scope * Statement * SemanticError list =
        match statement with
        | Expression (Call call) ->
            let newContext, result = funcCallAnalyzer call context
            newContext, Expression(Call call), result |> getError
        | Initialization i ->
            let newContext, result =
                initializationAnalyzer i context

            newContext, Initialization i, result |> getError
        | FuncDeclaration func ->
            let newContext, result =
                functionDeclAnalyzer func context

            newContext, FuncDeclaration func, result |> getError

        | st -> context, st, []

    let rec visitStatements stList context (errors: SemanticError list) =
        match stList with
        | cur :: tail ->
            let newContext, _, error = analyze cur context
            visitStatements tail newContext (errors @ error)
        | [] -> context, errors

    let scope =
        Global
            { Variables = ImmutableDictionary.Empty
              Functions = ImmutableDictionary.Empty }

    let context, errors =
        visitStatements statements scope []

    statements, context, errors
