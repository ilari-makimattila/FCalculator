module FCalculator.Tests.Algebra

open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateString is given a valid expression`` ()=
    
    [<Test>] member this.
        ``when expression is 1 + 2 answer is 3`` ()=
            EvaluateString "1 + 2" |> should equal 3m

    [<Test>] member this.
        ``when expression is 1 - 2 answer is -1`` ()=
            EvaluateString "1 - 2" |> should equal -1m
            
     [<Test>] member this.
        ``when expression is 2 * 2 answer is 4`` ()=
            EvaluateString "2 * 2" |> should equal 4m
            
     [<Test>] member this.
        ``when expression is 3 / 2 answer is 1.5`` ()=
            EvaluateString "3 / 2" |> should equal 1.5m
            
     [<Test>] member this.
        ``when expression is 6 % 2 answer is 0`` ()=
            EvaluateString "6 % 2" |> should equal 0m
                  
     [<Test>] member this.
        ``when expression is 7 % 2 answer is 1`` ()=
            EvaluateString "7 % 2" |> should equal 1m
     
     [<Test>] member this.
        ``when expression is 2 * 2 / 1 + 2 * 3 - 6 answer is 4`` ()=
            EvaluateString "2 * 2 / 1 + 2 * 3 - 6" |> should equal 4m
            
     [<Test>] member this.
        ``when expression is (1 + 2) answer is 3`` ()=
            EvaluateString "(1 + 2)" |> should equal 3m
        
     [<Test>] member this.
        ``when expression is (1 + 2) * 3 answer is 9`` ()=
            EvaluateString "(1 + 2) * 3" |> should equal 9m
            
     [<Test>] member this.
        ``when expression is (1 + 1) * (1 + 1) answer is 4`` ()=
            EvaluateString "(1 + 1) * (1 + 1)" |> should equal 4m
                
     [<Test>] member this.
        ``when expression is 2 * 2 / (1 + 2) * 3 - 6 answer is -2`` ()=
            EvaluateString "2 * 2 / (2 + 2) * 3 - 6" |> should equal -3m
     
     [<Test>] member this.
        ``when expression is ((1 + 2) * 3) answer is 9`` ()=
            EvaluateString "((1 + 2) * 3)" |> should equal 9m
      
     [<Test>] member this.
        ``when expression is ((1 + 2) * (3 - 1)) / 2 answer is 3`` ()=
            EvaluateString "((1 + 2) * (3 - 1)) / 2" |> should equal 3m