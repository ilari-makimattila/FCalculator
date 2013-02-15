module FCalculator.Tests.Relation

open NUnit.Framework
open FsUnit
open FCalculator.Evaluator

[<TestFixture>]
type ``Given EvaluateExpression is given a valid relation expression`` ()=

    [<Test>] member x.
        ``when expression is 1 <= 1 answer is true`` ()=
            EvaluateExpression "1 <= 1" |> should equal true
    
    [<Test>] member x.
        ``when expression is 1 >= 1 answer is true`` ()=
            EvaluateExpression "1 >= 1" |> should equal true
     
    [<Test>] member x.
        ``when expression is 1 <= 2 answer is true`` ()=
            EvaluateExpression "1 <= 2" |> should equal true
            
    [<Test>] member x.
        ``when expression is 2 >= 1 answer is true`` ()=
            EvaluateExpression "2 >= 1" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 >= 2 answer is false`` ()=
            EvaluateExpression "1 >= 2" |> should equal false 
            
    [<Test>] member x.
        ``when expression is 2 <= 1 answer is false`` ()=
            EvaluateExpression "2 <= 1" |> should equal false
            
    
    
    
    [<Test>] member x.
        ``when expression is 1 < 1 answer is false`` ()=
            EvaluateExpression "1 < 1" |> should equal false
    
    [<Test>] member x.
        ``when expression is 1 > 1 answer is false`` ()=
            EvaluateExpression "1 > 1" |> should equal false
     
    [<Test>] member x.
        ``when expression is 1 < 2 answer is true`` ()=
            EvaluateExpression "1 < 2" |> should equal true
            
    [<Test>] member x.
        ``when expression is 2 > 1 answer is true`` ()=
            EvaluateExpression "2 > 1" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 > 2 answer is false`` ()=
            EvaluateExpression "1 > 2" |> should equal false 
            
    [<Test>] member x.
        ``when expression is 2 < 1 answer is false`` ()=
            EvaluateExpression "2 < 1" |> should equal false
            
   