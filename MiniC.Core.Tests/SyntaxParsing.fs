module MiniC.Core.Tests.SyntaxParsing

open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.AST
open NUnit.Framework
open FParsec
open FsUnit
open MiniC.Core.Combinators.Syntax


let runParser input parser = runParserOnString parser "" "some" input

let toResult<'a, 'b> (value: ParserResult<'a, 'b>) : Result<_, _> =
    value
    |> function
        | Success (v, _, _) -> Result.Ok v
        | Failure (x, _, _) -> Result.Error x


[<Test>]
let ``Parse arguments definition`` () =
    let input = "int a, bool b, float c, int d"
    let parser = argsParser

    let expect: Parameter list =
        [ paramD ("a", IntL)
          paramD ("b", BoolL)
          paramD ("c", FloatL)
          paramD ("d", IntL) ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse one argument definition`` () =
    let input = "int a"
    let parser = argsParser

    let expect: Parameter list = [ paramD ("a", IntL) ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse variable declaration`` () =
    let input = "int a;"
    let parser = simpleVar

    let expect: Statement =
        VarDeclaration <| varD ("a", IntL)

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse variable initialisation`` () =
    let input = "int a = 4;"
    let parser = simpleInit

    let expect: Statement =
        Initialization(varD ("a", IntL), intLiteral 4)

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse int literal as expression`` () =
    let input = "4"
    let parser = expressionParser

    let expect: Expression = intLiteral 4

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse float literal as expression`` () =
    let input = "0.5"
    let parser = expressionParser

    let expect: Expression = floatLiteral 0.5

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with three args as expression`` () =
    let input = "func(a, 4, true);"
    let parser = expressionParser

    let expect: Expression =
        Call
            { FuncName = "func"
              Arguments =
                [ Identifier "a"
                  intLiteral 4
                  boolLiteral true ] }

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with two args without whitespaces as expression`` () =
    let input = "func(true,false);"
    let parser = expressionParser

    let expect: Expression =
        funcCall ("func", [ boolLiteral true; boolLiteral false ])

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with zero args as expression`` () =
    let input = "func();"
    let parser = expressionParser

    let expect: Expression = funcCall ("func", [])

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse statement block`` () =
    let input =
        "
        {
        ;
        ;
        ;
        }
        "

    let parser = statementParser

    let expect =
        Statement.Block [ Statement.Empty
                          Statement.Empty
                          Statement.Empty ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect


[<Test>]
let ``Parse function declaration`` () =
    let input =
        "void function(int a, bool b)
        {
        ;
        ;
        ;
        }
        "

    let parser = statementParser

    let func: FunctionDecl =
        { Name = "function"
          ReturnType = VoidL
          Parameters =
            [ paramD ("a", IntL)
              paramD ("b", BoolL) ] }

    let expect =
        FuncDeclaration(
            func,
            [ Statement.Empty
              Statement.Empty
              Statement.Empty ]
        )

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call inside function body`` () =
    let input =
        "void function(int a, bool b)
        {
        return function(24, true);
        }
        "

    let parser = statementParser

    let func: FunctionDecl =
        { Name = "function"
          ReturnType = VoidL
          Parameters =
            [ paramD ("a", IntL)
              paramD ("b", BoolL) ] }

    let expect =
        FuncDeclaration(func, [ Return <| funcCall ("function", [ intLiteral 24; boolLiteral true ]) ])

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect


[<Test>]
let ``Parse nested statement blocks`` () =
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

    let parser = statementParser

    let func1: FunctionDecl =
        { Name = "function"
          ReturnType = VoidL
          Parameters =
            [ paramD ("a", IntL)
              paramD ("b", BoolL) ] }

    let func2: FunctionDecl =
        { Name = "example"
          ReturnType = IntL
          Parameters = [ paramD ("value", BoolL) ] }

    let expect =
        FuncDeclaration(
            func1,
            [ FuncDeclaration(func2, [ Return << Literal << IntNumber <| 23 ])
              Block [ Initialization(
                          { TypeDecl = IntL; Name = "c" },
                          funcCall ("example", [ intLiteral 24; boolLiteral true ])
                      ) ] ]
        )

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse declarations as program`` () =
    let input =
        "
        int abc;
        int def = 4;
        void function(int a, bool b){;}
        "

    let parser = programParser

    let func: FunctionDecl =
        { Name = "function"
          ReturnType = VoidL
          Parameters =
            [ { TypeDecl = IntL; Name = "a" }
              { TypeDecl = BoolL; Name = "b" } ] }

    let expect =
        [ VarDeclaration <| varD ("abc", IntL)
          Initialization(varD ("def", IntL), intLiteral 4)
          FuncDeclaration(func, [ Statement.Empty ]) ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect
