module MiniC.Core.Tests.SyntaxParsing

open FSharpPlus
open FSharpPlus.Data
open MiniC.Core.AST
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
        [ { Name = "a"; TypeDecl = TypeL IntL }
          { Name = "b"; TypeDecl = TypeL BoolL }
          { Name = "c"; TypeDecl = TypeL FloatL }
          { Name = "d"; TypeDecl = TypeL IntL } ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse one argument definition`` () =
    let input = "int a"
    let parser = argsParser

    let expect: Parameter list =
        [ { Name = "a"
            TypeDecl = TypeLiteral.TypeL IntL } ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse variable declaration`` () =
    let input = "int a;"
    let parser = simpleVar

    let expect: Statement =
        Statement.VarDeclaration
            { Name = "a"
              TypeDecl = TypeLiteral.TypeL IntL }

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse variable initialisation`` () =
    let input = "int a = 4;"
    let parser = simpleInit

    let expect: Statement =
        Statement.Initialization(
            { Name = "a"
              TypeDecl = TypeLiteral.TypeL IntL },
            Literal << IntNumber <| 4
        )

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse int literal as expression`` () =
    let input = "4"
    let parser = expressionParser

    let expect: Expression = Literal << IntNumber <| 4

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse float literal as expression`` () =
    let input = "0.5"
    let parser = expressionParser

    let expect: Expression =
        Literal.FloatNumber 0.5 |> Expression.Literal

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with three args as expression`` () =
    let input = "func(a, 4, true);"
    let parser = expressionParser

    let expect =
        Expression.Call
            { FuncName = "func"
              Arguments =
                [ Expression.Identifier "a"
                  Expression.Literal <| Literal.IntNumber 4
                  Expression.Literal <| Literal.Boolean true ] }

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with two args without whitespaces as expression`` () =
    let input = "func(true,false);"
    let parser = expressionParser

    let expect: Expression =
        Expression.Call
            { FuncName = "func"
              Arguments =
                [ Expression.Literal <| Literal.Boolean true
                  Expression.Literal <| Literal.Boolean false ] }

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect

[<Test>]
let ``Parse function call with zero args as expression`` () =
    let input = "func();"
    let parser = expressionParser

    let expect =
        Expression.Call { FuncName = "func"; Arguments = [] }

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
          ReturnType = TypeLiteral.TypeL TypeL.VoidL
          Parameters =
            [ { TypeDecl = TypeLiteral.TypeL TypeL.IntL
                Name = "a" }
              { TypeDecl = TypeLiteral.TypeL TypeL.BoolL
                Name = "b" } ] }

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
          ReturnType = TypeLiteral.TypeL TypeL.VoidL
          Parameters =
            [ { TypeDecl = TypeLiteral.TypeL TypeL.IntL
                Name = "a" }
              { TypeDecl = TypeLiteral.TypeL TypeL.BoolL
                Name = "b" } ] }

    let expect =
        FuncDeclaration(
            func,
            [ Statement.Return
              <| Expression.Call
                  { FuncName = "function"
                    Arguments =
                      [ Expression.Literal <| Literal.IntNumber 24
                        Expression.Literal <| Literal.Boolean true ] } ]
        )

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
          ReturnType = TypeL VoidL
          Parameters =
            [ { TypeDecl = TypeL IntL; Name = "a" }
              { TypeDecl = TypeL BoolL; Name = "b" } ] }

    let func2: FunctionDecl =
        { Name = "example"
          ReturnType = TypeL IntL
          Parameters =
            [ { TypeDecl = TypeL BoolL
                Name = "value" } ] }

    let expect =
        FuncDeclaration(
            func1,
            [ FuncDeclaration(func2, [ Return << Literal << IntNumber <| 23 ])
              Block [ Initialization(
                          { TypeDecl = TypeL IntL; Name = "c" },
                          Call
                              { FuncName = "example"
                                Arguments =
                                  [ Literal <| IntNumber 24
                                    Literal <| Boolean true ] }
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
          ReturnType = TypeL VoidL
          Parameters =
            [ { TypeDecl = TypeL IntL; Name = "a" }
              { TypeDecl = TypeL BoolL; Name = "b" } ] }

    let expect =
        [ VarDeclaration { TypeDecl = TypeL IntL; Name = "abc" }
          Initialization({ TypeDecl = TypeL IntL; Name = "def" }, Literal << IntNumber <| 4)
          FuncDeclaration(func, [ Statement.Empty ]) ]

    runParser input parser
    |> toResult
    |> Result.get
    |> should equal expect
