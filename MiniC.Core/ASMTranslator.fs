module MiniC.Core.ASMTranslator

open FSharpPlus
open FSharpPlus.Data
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

let returnValue = List.singleton << AToken.Return

let returnIdentifier id register =
    [ Mov(ValueHolder.Register register, Variable id)
      AToken.Return(StackOperand.Register register) ]

let save register = Push(StackOperand.Register register)

let load register = Pop register



let funcResultVariable (scope: Scope) id =
    let head, _ = scope.getFunction id |> Result.get

    resultVariable head

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

let translateFunction (func: Function) (statementSolver: Statement -> AToken list) : AToken list =
    let head, body = func

    let parameters: VariableDecl list =
        head.Parameters |> List.map (fun p -> p.Name, asmType p.TypeDecl)

    let instructions: FunctionBlock =
        body |> List.collect statementSolver


    [ (AToken.Function(head.Name, parameters, instructions)) ]

let rec translateStatement (globalScope: Scope) =
    function
    | Expression (Call func) -> translateCall globalScope func
    | Initialization i -> failwith "not implemented"
    | VarDeclaration var -> []
    | FuncDeclaration func -> translateFunction func (translateStatement globalScope)
    | st -> failwith $"not allowed %A{st}"

let programTranslator statements globalScope : AToken list * VariableDecl list =
    let variables = collectVariables statements

    let tokens =
        statements |> List.collect (translateStatement globalScope)


    tokens, variables
