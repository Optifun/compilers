module MiniC.Core.ASMTranslator

open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.ASMTokens
open MiniC.Core.AST
open MiniC.Core.Analyzers

let rec collectVariables statements : ASMTokens.Variable list =
    statements
    |> List.choose (
        function
        | VarDeclaration var -> Some [ var.Name ]
        | Initialization (var, _) -> Some [ var.Name ]
        | FuncDeclaration (head, body) ->
            let parameters =
                head.Parameters |> List.map (fun p -> p.Name)

            let localVariables = collectVariables body
            Some <| parameters @ localVariables
        | _ -> None
    )
    |> List.collect id


let translateCall (func: FunctionCall) (returnType: TypeLiteral) : AToken list =
    let goto = [ Label func.FuncName ]

    let saveRegisters =
        Register.all () |> List.map (StackOperand.Register >> Push)

    let loadRegisters = Register.all () |> List.map Pop

    let pushLiteral =
        List.singleton << Push << StackOperand.Literal

    let pushIdentifier id =
        [ Mov(ValueHolder.Register AX, Variable id)
          Push(StackOperand.Register AX) ]

    let passArguments =
        func.Arguments
        |> List.collect (
            function
            | Literal (Boolean b) -> pushLiteral <| Bool b
            | Literal (IntNumber i) -> pushLiteral <| HexInt i
            | Literal (FloatNumber f) -> pushLiteral <| HexFloat f
            | Identifier id -> pushIdentifier id
            | _ -> failwith "not implemented"
        )

    let getResult =
        if returnType <> TypeLiteral.VoidL then
            [ Pop DX ]
        else
            []

    saveRegisters @ passArguments @ goto @ getResult @ loadRegisters


let translateFunction (func: Function) (statementSolver: Statement -> AToken list) : AToken list =
    let head, body = func
    let label = Label head.Name


    let popParameters =
        head.Parameters
        |> List.rev
        |> List.collect (fun p ->
            [ Pop DX
              Mov(ValueHolder.Variable p.Name, Register DX) ]
        )

    let instructions =
        body |> List.collect statementSolver

    [ label
      FunctionBlock <| popParameters @ instructions ]

let rec translateStatement (globalScope: Context) =
    function
    | Expression (Call func) ->
        let head, _ =
            globalScope.getFunction func.FuncName |> Result.get

        translateCall func head.ReturnType
    | Initialization i -> failwith "not implemented"
    | VarDeclaration var -> []
    | FuncDeclaration func -> translateFunction func (translateStatement globalScope)
    | st -> failwith "not allowed"

let programTranslator statements globalScope : AToken list * ASMTokens.Variable list =
    let variables = collectVariables statements

    let tokens =
        statements |> List.collect (translateStatement globalScope)


    tokens, variables
