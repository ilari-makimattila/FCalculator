module FCalculator.Tests.Functions

open System
open NUnit.Framework
open FsUnit
open FCalculator.Evaluator

[<TestFixture>]
type ``Given EvaluateExpressionWithFunctions has a map of funcs and a valid expression`` ()=    
    let f = new Map<string, obj list -> obj>([ 
                                             ("square", 
                                              fun o -> box(Convert.ToDecimal(o.Head) * Convert.ToDecimal(o.Head)));
                                             ("mul",
                                              fun o -> box(Convert.ToDecimal(o.[0]) * Convert.ToDecimal(o.[1]))) ])

    [<Test>] member x.
        ``when expression is square(2) answer is 4`` ()=    
            EvaluateExpressionWithFunctions f "square(2)" |> should equal 4.0m
    
    [<Test>] member x.
        ``when expression is square(1 + 1) answer is 4`` ()=    
            EvaluateExpressionWithFunctions f "square(1 + 1)" |> should equal 4.0m
            
    [<Test>] member x.
        ``when expression is mul(3, 2) answer is 6`` ()=    
            EvaluateExpressionWithFunctions f "mul(3, 2)" |> should equal 6.0m
            
    [<Test>] member x.
        ``when expression is mul(3, 9) answer is 27`` ()=    
            EvaluateExpressionWithFunctions f "mul(3, 9)" |> should equal 27.0m
            
    [<Test>] member x.
        ``when expression is mul(3, 9) - square(5) answer is 2`` ()=    
            EvaluateExpressionWithFunctions f "mul(3, 9) - square(5)" |> should equal 2.0m
            
    [<Test>] member x.
        ``when expression is if (0, 1, 2) then the answer is 2`` ()=
            EvaluateExpression "if (0, 1, 2)" |> should equal 2.0m
            
    [<Test>] member x.
        ``when expression is in (1 + 1, 1, 2) then the answer is true`` ()=
            EvaluateExpression "in (1 + 1, 1, 2)" |> should equal true
            
    [<Test>] member x.
        ``when expression is not(false) then the answer is true`` ()=
            EvaluateExpression "not(false)" |> should equal true
            
            
