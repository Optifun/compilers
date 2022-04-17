﻿module MiniC.Core.Analyzers

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

let errorL (err: 'a) : Result<'b, 'a list> = err |> List.singleton |> Result.Error

let getError =
    function
    | Result.Ok _ -> []
    | Result.Error e -> [ e ]

let liftAnalysisError (result: Result<'T, 'a>) (lift: 'T -> Statement) : Result<Statement, 'a list> =
    match result with
    | Result.Ok e -> Result.Ok <| lift e
    | Result.Error err -> errorL err

let liftAnalysis (result: Result<'T, 'a>) (lift: 'T -> Statement) =
    match result with
    | Result.Ok e -> Result.Ok <| lift e
    | Result.Error err -> Result.Error err


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

let rec expressionAnalyzer (node: Expression) (context: Scope) : Result<Expression, SemanticError> =

    let funcCallAnalyzer (node: FunctionCall) (context: Scope) : Scope * Result<FunctionCall, SemanticError> =

        let checkParamTypes (param: Parameter, arg: Expression) =
            let result =
                expressionAnalyzer arg context
                |> Result.bind (fun exp -> matchTypeWith param.TypeDecl exp context)
                |> Result.bindError (fun _ -> Result.Error <| ParameterTypeMismatch(param, arg))

            match result with
            | Result.Ok _ -> true
            | Result.Error _ -> false

        let checkFunction name =
            monad.strict {
                let! func = context.getFunction name
                let f, _ = func

                if (f.Parameters.Length <> node.Arguments.Length) then
                    return func, false
                else
                    return func, List.zip f.Parameters node.Arguments |> List.forall checkParamTypes
            }

        let response =
            checkFunction node.FuncName
            |> Result.mapError (fun _ -> UnknownFunctionCall(node.FuncName))
            |> Result.bind (
                function
                | _, succeed when succeed -> Result.Ok node
                | (f, _), _ -> Result.Error <| FunctionCallWrongParameters(f, node)
            )

        context, response

    let rec visit expr =
        expr
        |> function
            | Literal literal -> Result.Ok expr
            | Call funcCall ->
                if not (isOk <| context.getFunction funcCall.FuncName) then
                    Result.Error <| UnknownFunctionCall funcCall.FuncName
                else
                    funcCallAnalyzer funcCall context
                    |> function
                        | _, Result.Ok _ -> Result.Ok expr
                        | _, Result.Error err -> Result.Error err

            | Identifier id ->
                if (isOk <| context.getVariable id) then
                    Result.Ok expr
                else
                    Result.Error <| UnknownIdentifier id
            | _ -> Result.Ok expr

    visit node


let initializationAnalyzer
    (node: Initialization)
    (context: Scope)
    : Scope * Result<Initialization, SemanticError list> =
    let var, value = node

    let newContext = context.registerVariable var

    let result =
        expressionAnalyzer value context
        |> Result.bind (fun exp -> matchTypeWith var.TypeDecl exp context)
        |> Result.bindError (
            function
            | FunctionCallWrongParameters (d, b) -> errorL <| FunctionCallWrongParameters(d, b)
            | ExpectedType _ -> errorL <| TypeMismatch(var, value)
            | err ->
                Result.Error [ err
                               TypeMismatch(var, value) ]
        )


    match result with
    | Result.Ok _ -> newContext, Result.Ok node
    | Result.Error error -> context, Result.Error error

let functionDeclAnalyzer (func: Function) (context: Scope) : Scope * Result<Function, SemanticError list> =
    let decl, _ = func

    if (isOk <| context.getVariable decl.Name
        || isOk <| context.getFunction decl.Name) then
        context, errorL <| IdentifierCollision(decl.Name, "Identifier already in use")
    else
        context.registerFunction func, Result.Ok func


let programAnalyzer (statements: Statement list) =

    let analyze (statement: Statement) (context: Scope) : Scope * Result<Statement, SemanticError list> =
        match statement with
        | Expression e ->
            let result = expressionAnalyzer e context
            context, liftAnalysisError result Expression
        | Initialization i ->
            let newContext, result =
                initializationAnalyzer i context

            newContext, liftAnalysis result Initialization
        | FuncDeclaration func ->
            let newContext, result =
                functionDeclAnalyzer func context

            newContext, liftAnalysis result FuncDeclaration

        | st -> context, Result.Ok st

    let rec visitStatements
        (stList: Statement list)
        (context: Scope)
        (errors: SemanticError list)
        : Statement list * Scope * SemanticError list =
        match stList with
        | cur :: tail ->
            let newContext, result = analyze cur context

            match result with
            | Result.Ok statement ->
                let _statements, _scope, _errors =
                    visitStatements tail newContext errors

                [ statement ] @ _statements, _scope, _errors
            | Result.Error err -> visitStatements tail newContext (errors @ err)
        | [] -> stList, context, errors

    let scope =
        Global
            { Variables = ImmutableDictionary.Empty
              Functions = ImmutableDictionary.Empty }

    visitStatements statements scope []
