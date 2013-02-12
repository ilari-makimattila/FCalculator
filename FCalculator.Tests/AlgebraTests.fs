module FCalculator.Tests.Algebra

open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateString is given a valid expression`` ()=
    
    [<Test>] member this.
        ``when expression is 1 + 2 answer is 3.0`` ()=
            EvaluateString "1 + 2" |> should equal 3.0

    [<Test>] member this.
        ``when expression is 1 - 2 answer is -1.0`` ()=
            EvaluateString "1 - 2" |> should equal -1.0
            
     [<Test>] member this.
        ``when expression is 2 * 2 answer is 4.0`` ()=
            EvaluateString "2 * 2" |> should equal 4.0
            
     [<Test>] member this.
        ``when expression is 3 / 2 answer is 1.5`` ()=
            EvaluateString "3 / 2" |> should equal 1.5
            
     [<Test>] member this.
        ``when expression is 2 * 2 / 1 + 2 * 3 - 6 answer is 4.0`` ()=
            EvaluateString "2 * 2 / 1 + 2 * 3 - 6" |> should equal 4.0
            
     [<Test>] member this.
        ``when expression is (1 + 2) answer is 3.0`` ()=
            EvaluateString "(1 + 2)" |> should equal 3.0
        
     [<Test>] member this.
        ``when expression is (1 + 2) * 3 answer is 9.0`` ()=
            EvaluateString "(1 + 2) * 3" |> should equal 9.0
            
     [<Test>] member this.
        ``when expression is (1 + 1) * (1 + 1) answer is 4.0`` ()=
            EvaluateString "(1 + 1) * (1 + 1)" |> should equal 4.0
                
     [<Test>] member this.
        ``when expression is 2 * 2 / (1 + 2) * 3 - 6 answer is -2.0`` ()=
            EvaluateString "2 * 2 / (1 + 2) * 3 - 6" |> should equal -2.0
     
     [<Test>] member this.
        ``when expression is ((1 + 2) * 3) answer is 9.0`` ()=
            EvaluateString "((1 + 2) * 3)" |> should equal 9.0
      
     [<Test>] member this.
        ``when expression is ((1 + 2) * (3 - 1)) / 2 answer is 2.0`` ()=
            EvaluateString "((1 + 2) * (3 - 1)) / 2" |> should equal 3.0