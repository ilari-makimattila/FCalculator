module FCalculator.Tests.Boolean

open NUnit.Framework
open FsUnit
open FCalculator.Evaluator
open FCalculator.Parser
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateExpression is given a valid boolean expression`` ()=

    [<Test>] member x.
        ``when expression is 1 = 1 answer is true`` ()=
            EvaluateExpression "1 = 1" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 = 2 answer is false`` ()=
            EvaluateExpression "1 = 2" |> should equal false
            
    [<Test>] member x.
        ``when expression is 1 <> 2 answer is true`` ()=
            EvaluateExpression "1 <> 2" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 <> 1 answer is false`` ()=
            EvaluateExpression "1 <> 1" |> should equal false
            
    [<Test>] member x.
        ``when expression is 2 and 2 answer is true`` ()=
            EvaluateExpression "2 and 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 1 and 2 answer is true`` ()=
            EvaluateExpression "1 and 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 0 and 0 answer is false`` ()=
            EvaluateExpression "0 and 0" |> should equal false
            
    [<Test>] member x.
        ``when expression is 2 or 2 answer is true`` ()=
            EvaluateExpression "2 or 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 1 or 2 answer is true`` ()=
            EvaluateExpression "1 or 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 0 or 0 answer is false`` ()=
            EvaluateExpression "0 or 0" |> should equal false
            
    [<Test>] member x.
        ``when expression is 1 or 0 answer is true`` ()=
            EvaluateExpression "1 or 0" |> should equal true
    
    [<Test>] member x.
        ``when expression is not(false) then the answer is true`` ()=
            EvaluateExpression "not(false)" |> should equal true
            
    [<Test>] member x.
        ``when expression is not false then the answer is true`` ()=
            EvaluateExpression "not false" |> should equal true
    
    [<Test>] member x.
        ``when expression is "true or false and true" the and operator should have higher priority`` ()=
            ParseExpression "true or false and true" 
            |> should equal (LogicalOr(Variable("true"), LogicalAnd(Variable("false"), Variable("true"))))