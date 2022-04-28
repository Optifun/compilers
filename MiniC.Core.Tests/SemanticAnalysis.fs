module MiniC.Core.Tests.SemanticAnalysis

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open MiniC.Core.Error
open NUnit.Framework
open MiniC.Core.Analyzers
open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.AST
open NUnit.Framework
open FParsec
open FsUnit
open MiniC.Core.Combinators.Syntax


let runParser input parser = runParserOnString parser "" "some" input

let toImmutable (d: IDictionary<'a, 'c>) : ImmutableDictionary<'a, 'c> = d.ToImmutableDictionary()

let toResult<'a, 'b> (value: ParserResult<'a, 'b>) : Result<_, _> =
    value
    |> function
        | Success (v, _, _) -> Result.Ok v
        | Failure (x, _, _) -> Result.Error x


let constructContext (variables: Variable list) (functions: Function list) : Context =
    let vars =
        variables |> Seq.map (fun v -> v.Name, v) |> dict |> toImmutable

    let funcs =
        functions
        |> Seq.map (fun (fd, fb) -> fd.Name, (fd, fb))
        |> dict
        |> toImmutable

    { Variables = vars; Functions = funcs }

[<Test>]
let ``Initialisation type mismatch`` () =
    let input =
        "
        int abc = false;
        "

    let _statements: Statement list = []

    let _scope = constructContext [] [] |> Global

    let _errors: SemanticError list =
        [ TypeMismatch({ Name = "abc"; TypeDecl = IntL }, boolLiteral false) ]

    let statements, scope, errors =
        runParser input programParser
        |> toResult
        |> Result.map programAnalyzer
        |> Result.get

    statements |> should equal _statements
    errors |> should equal _errors
    scope |> should equal _scope


[<Test>]
let ``Variables initialization with literals`` () =
    let input =
        "
        int abc = 2;
        float def = -1.0;
        bool ghi = true;
        "

    let _statements =
        [ Initialization(varD ("abc", IntL), intLiteral 2)
          Initialization(varD ("def", FloatL), floatLiteral -1.0)
          Initialization(varD ("ghi", BoolL), boolLiteral true) ]

    let _scope =
        constructContext [ varD ("abc", IntL)
                           varD ("def", FloatL)
                           varD ("ghi", BoolL) ] []
        |> Global

    let _errors: SemanticError list = []

    let statements, scope, errors =
        runParser input programParser
        |> toResult
        |> Result.map programAnalyzer
        |> Result.get

    statements |> should equal _statements
    errors |> should equal _errors
    scope |> should equal _scope

[<Test>]
let ``Variable initialization with another variable`` () =
    let input =
        "
        int abc = 2;
        int def = abc;
        "

    let _statements =
        [ Initialization(varD ("abc", IntL), intLiteral 2)
          Initialization(varD ("def", IntL), Identifier "abc") ]

    let _scope =
        constructContext [ varD ("abc", IntL)
                           varD ("def", IntL) ] []
        |> Global

    let _errors: SemanticError list = []

    let statements, scope, errors =
        runParser input programParser
        |> toResult
        |> Result.map programAnalyzer
        |> Result.get

    statements |> should equal _statements
    errors |> should equal _errors
    scope |> should equal _scope


[<Test>]
let ``Function calling with correct parameters`` () =
    let input =
        "
        int f(int n) {return 42;}
        int abc = f(2);
        "

    let funcCall =
        Call
            { FuncName = "f"
              Arguments = [ intLiteral 2 ] }

    let func =
        funcD ("f", IntL, [ varD ("n", IntL) ]), [ Statement.Return <| intLiteral 42 ]

    let variable = varD ("abc", IntL)

    let _statements =
        [ FuncDeclaration func
          Initialization(variable, funcCall) ]

    let _scope =
        constructContext [ variable ] [ func ] |> Global

    let _errors: SemanticError list = []

    let statements, scope, errors =
        runParser input programParser
        |> toResult
        |> Result.map programAnalyzer
        |> Result.get

    statements |> should equal _statements
    errors |> should equal _errors
    scope |> should equal _scope

[<Test>]
let ``Calling function with incorrect parameters count`` () =
    let input =
        "
        int f(int n) {return 42;}
        int abc = f(2, 4);
        "

    let funcCall =
        { FuncName = "f"
          Arguments = [ intLiteral 2; intLiteral 4 ] }

    let functionDecl =
        funcD ("f", IntL, [ varD ("n", IntL) ])

    let func =
        functionDecl, [ Statement.Return <| intLiteral 42 ]

    let variable = varD ("abc", IntL)

    let _statements: Statement list =
        [ FuncDeclaration func ]

    let _scope = constructContext [] [ func ] |> Global

    let _errors: SemanticError list =
        [ FunctionCallWrongParameters(functionDecl, funcCall) ]

    let statements, scope, errors =
        runParser input programParser
        |> toResult
        |> Result.map programAnalyzer
        |> Result.get

    statements |> should equal _statements
    errors |> should equal _errors
    scope |> should equal _scope


[<Test>]
let ``Define function inside another function`` () =
    let input =
        "void function(int a, bool b)
        {
            int example(bool value)
            {
                return 23;
            }
            {
                int c = example(24, true);
            }
        }
        "

    let funcD1: FunctionDecl =
        { Name = "function"
          ReturnType = VoidL
          Parameters = [ varD ("a", IntL); varD ("b", BoolL) ] }

    let funcD2: FunctionDecl =
        { Name = "example"
          ReturnType = IntL
          Parameters = [ varD ("value", BoolL) ] }

    let func2: Function =
        funcD2, [ Return << Literal << IntNumber <| 23 ]

    let func1Body =
        [ FuncDeclaration(func2)
          Block [ Initialization(
                      { TypeDecl = IntL; Name = "c" },
                      funcCall ("example", [ intLiteral 24; boolLiteral true ])
                  ) ] ]

    let func1 = funcD1, func1Body

    let _statements =
        [ FuncDeclaration(funcD1, func1Body) ]

    let _scope = constructContext [] [ func1 ] |> Global

    let _errors: SemanticError list = []

    //    let statements, scope, errors =
    monad.strict {
        let! AST = runParser input programParser |> toResult
        let statements, scope, errors = programAnalyzer AST

        statements |> should equal _statements
        errors |> should equal _errors
        scope |> should equal _scope
    }
    |> monad.Run
    |> ignore
