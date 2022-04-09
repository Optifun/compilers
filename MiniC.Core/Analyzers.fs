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


let rec visitScope (scope: Scope) (visitor: Context -> Identifier -> Result<'elem, exn>) id : Result<'elem, exn> =
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


let funcRetType (func: Function) =
    let decl, body = func
    decl.ReturnType



let initializationAnalyzer (node: Initialization) (context: Scope) : Scope * Result<Initialization, SemanticError> =
    let var, value = node

    let newContext = context.registerVariable var

    match var.TypeDecl, value with
    | varType, Literal literal when varType = literal.GetTypeLiteral() -> newContext, Result.Ok node
    | varType, Call funcCall when
        context.getFunction funcCall.FuncName
        |> Result.map (fun f -> varType = funcRetType f)
        |> Result.defaultValue false
        ->
        newContext, Result.Ok node
    | _ -> context, Result.Error <| TypeMismatch(var, value)

let funcCallAnalyzer (node: FunctionCall) (context: Scope) : Scope * Result<FunctionCall, SemanticError> =

    let checkParamTypes (param: Parameter, arg: Expression) =
        match param.TypeDecl, arg with
        | pType, Literal l when l.GetTypeLiteral() = pType -> true
        | pType, Identifier id when
            context.getVariable id
            |> Result.map (fun v -> pType = v.TypeDecl)
            |> Result.defaultValue false
            ->
            true
        | pType, Call funcCall when
            context.getFunction funcCall.FuncName
            |> Result.map (fun f -> pType = funcRetType f)
            |> Result.defaultValue false
            ->
            true
        | _ -> false


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
