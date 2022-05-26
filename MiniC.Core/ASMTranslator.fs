module MiniC.Core.ASMTranslator

open System
open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.ASMTokens
open MiniC.Core.ASMTokens
open MiniC.Core.ASMTokens
open MiniC.Core.ASMTokens
open MiniC.Core.AST
open MiniC.Core.Analyzers

let asmType =
    function
    | IntL
    | BoolL -> BSize.WORD
    | FloatL -> BSize.DWORD
    | _ -> failwith "not supported type"

let resultVariable func : VariableDecl option =
    if (func.ReturnType <> VoidL) then
        Some(func.Name, asmType func.ReturnType)
    else
        None

let rec collectVariables statements : ASMTokens.VariableDecl list =
    statements
    |> List.choose (
        function
        | VarDeclaration var -> Some [ var.Name, asmType var.TypeDecl ]
        | Initialization (var, _) -> Some [ var.Name, asmType var.TypeDecl ]
        | FuncDeclaration (head, body) ->
            let resultVar: VariableDecl list =
                resultVariable head
                |> Option.map (List.singleton << id)
                |> Option.defaultValue []

            let parameters =
                head.Parameters |> List.map (fun p -> p.Name, asmType p.TypeDecl)

            let localVariables = collectVariables body

            Some <| resultVar @ parameters @ localVariables
        | _ -> None
    )
    |> List.collect id


let asmLiteral: Literal -> ASMTokens.Literal =
    function
    | Literal.Boolean b -> Bool b
    | Literal.IntNumber i -> HexInt i
    | Literal.FloatNumber f -> HexFloat f
    | _ -> failwith "not allowed"

let saveRegisters =
    Register.all () |> List.map (StackOperand.Register >> Push)

let loadRegisters = Register.all () |> List.map Pop

let pushLiteral =
    List.singleton << Push << StackOperand.Literal

let popIdentifier id register =
    [ Pop <| DX
      Mov(ValueHolder.Variable id, Register register) ]

let pushIdentifier id register =
    [ Mov(ValueHolder.Register register, Variable id)
      Push(StackOperand.Register register) ]

let copyTo source destination register =
    [ Mov(ValueHolder.Register register, Variable source)
      Mov(ValueHolder.Variable destination, Register register) ]

let returnValue = List.singleton << AToken.Return

let returnIdentifier id register =
    [ Mov(ValueHolder.Register register, Variable id)
      AToken.Return(StackOperand.Register register) ]

let save register = Push(StackOperand.Register register)

let load register = Pop register



let funcResultVariable (scope: Scope) id =
    let head, _ = scope.getFunction id |> Result.get

    resultVariable head

/// Pushes saves all registers, parameters on stack,
let rec translateCall (scope: Scope) (func: FunctionCall) : AToken list =

    let preformCall (func: FunctionCall) (tempVariable: VariableDecl option) =
        let translateParameter param =
            param
            |> function
                | Literal l -> pushLiteral <| asmLiteral l
                | Identifier id -> [ save BX ] @ pushIdentifier id BX @ [ load BX ]
                | Call func -> translateCall scope func
                | _ -> failwith "not implemented"

        monad.plus {
            yield! saveRegisters

            yield! func.Arguments |> List.collect translateParameter

            yield! [ AToken.Call func.FuncName ]

            yield!
                tempVariable
                |> Option.map (fun (id, _) -> popIdentifier id DX @ loadRegisters @ pushIdentifier id DX)
                |> Option.defaultValue loadRegisters
        }


    let tempVariable =
        funcResultVariable scope func.FuncName

    preformCall func tempVariable

let returnFunctionCall scope funcCall =
    let callFunction = translateCall scope funcCall

    let getResult =
        funcResultVariable scope funcCall.FuncName
        |> Option.map (fun (name, _) -> pushIdentifier name DX)
        |> Option.defaultValue []

    callFunction @ getResult @ returnValue (StackOperand.Register DX)

let passResult variable funcName = copyTo funcName variable AX

let translateReturn (scope: Scope) =
    function
    | Literal l -> returnValue << StackOperand.Literal << asmLiteral <| l
    | Identifier id -> [ save BX ] @ returnIdentifier id BX @ [ load BX ]
    | Call call -> returnFunctionCall scope call
    | _ -> failwith "not implemented"

let translateFunction (func: Function) (statementSolver: Statement -> AToken list) : AToken list =
    let head, body = func

    let parameters: VariableDecl list =
        head.Parameters |> List.map (fun p -> p.Name, asmType p.TypeDecl)

    let instructions: FunctionBlock =
        body |> List.collect statementSolver


    [ (AToken.Function(head.Name, parameters, instructions)) ]

let translateInitialization scope variable value =
    value
    |> function
        | Call func ->
            let callFunction = translateCall scope func
            let getResult = popIdentifier variable.Name DX

            callFunction @ getResult
        | Literal l -> [ Mov(ValueHolder.Variable variable.Name, Value.Literal <| asmLiteral l) ]
        | Identifier d -> passResult variable.Name d
        | _ -> failwith "not allowed"

let rec translateStatement (globalScope: Scope) =
    function
    | Expression (Call func) -> translateCall globalScope func
    | Initialization (var, value) -> translateInitialization globalScope var value
    | VarDeclaration var -> []
    | Return value -> translateReturn globalScope value
    | FuncDeclaration func -> translateFunction func (translateStatement globalScope)
    | st -> failwith $"not allowed %A{st}"

let programTranslator statements globalScope : AToken list * VariableDecl Set =
    let variables =
        Set.ofList <| collectVariables statements

    let tokens =
        statements |> List.collect (translateStatement globalScope)


    tokens, variables

let rec stringify tokens =

    let printLiteral =
        function
        | Literal.HexFloat f -> f.ToString()
        | Literal.HexInt i -> i.ToString()
        | Literal.Bool b -> if b then "TRUE" else "FALSE"

    let printStackOP =
        function
        | StackOperand.Literal l -> printLiteral l
        | StackOperand.Register r -> r.ToString()

    let printValueHolder =
        function
        | ValueHolder.Register r -> r.ToString()
        | ValueHolder.Variable v -> v

    let printValue =
        function
        | Value.Register r -> r.ToString()
        | Value.Variable v -> v
        | Value.Literal l -> printLiteral l

    let printArguments arguments =
        arguments
        |> function
            | [] -> ""
            | args ->
                let content =
                    args |> List.map (fun (n, t) -> $"{n}: {t}") |> List.intersperse ", "

                [ "(" ] @ content @ [ ")" ] |> String.concat ""

    let tabify =
        List.map (fun str -> String.replicate 1 "\t" + str)

    tokens
    |> List.collect (
        function
        | AToken.Mov (src, dest) -> [ $"MOV {printValueHolder src} {printValue dest}" ]
        | AToken.Return st -> [ $"RETURN {printStackOP st}" ]
        | AToken.Call fn -> [ $"CALL {fn}" ]
        | AToken.Pop r -> [ $"POP {r}" ]
        | AToken.Push v -> [ $"PUSH {printStackOP v}" ]
        | AToken.Function (name, arguments, block) ->
            [ $"{name} {printArguments arguments} PROC FAR" ]
            @ tabify (stringify block) @ [ $"{name} ENDP"; "" ]
        | _ -> failwith "not implemented"
    )

let printTokens tokens = stringify tokens |> String.concat "\r\n"

let declareVariables (vars: VariableDecl Set) =
    vars
    |> Seq.map (fun (name, size) -> $"{name}\t{size}\t0;")
    |> String.concat "\r\n"

let printProgram tokens vars =
    """
.model LARGE;
"""
    + "\r\n\r\n.data\r\n"
    + declareVariables vars
    + "\r\n\r\n.code\r\n"
    + printTokens tokens
