module MiniC.Core.Analyzers

open System
open System.Collections.Generic
open System.Collections.Immutable
open FSharpPlus
open FParsec
open FSharpPlus
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
    member x.registerVariable var = { x with Variables = x.Variables.Add <| (var.Name, var) }

    member x.registerFunction (decl: FunctionDecl, block) =
        { x with Functions = x.Functions.Add <| (decl.Name, (decl, block)) }

    member x.hasFunction (id: Identifier) = Seq.contains id x.Functions.Keys
    member x.hasVariable (id: Identifier) = Seq.contains id x.Variables.Keys
    member x.hasIdentifier (id: Identifier) = x.hasFunction id || x.hasVariable id

    member x.getFunction =
        Result.protect <| fun (id: Identifier) -> x.Functions.[id]

    member x.getVariable =
        Result.protect <| fun (id: Identifier) -> x.Variables.[id]


let funcRetType (func: Function) =
    let decl, body = func
    decl.ReturnType



let initializationAnalyzer (node: Initialization) (context: Context) : Context * Result<Initialization, SemanticError> =
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

let funcCallAnalyzer (node: FunctionCall) (context: Context) : Context * Result<FunctionCall, SemanticError> =

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
