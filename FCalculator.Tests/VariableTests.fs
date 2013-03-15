module FCalculator.Tests.Variables

open System
open NUnit.Framework
open FsUnit
open FCalculator.Evaluator

[<TestFixture>]
type ``Given EvaluateExpressionWithVariables has a map of variables and a valid expression`` ()=    
    let m = new Map<string,Object>([ ("x", box 1); ("y", box 2) ])       
    
    [<Test>] member x.
        ``when expression is x = 1 answer is true`` ()=    
            EvaluateExpressionWithVariables m "x = 1" |> should equal true
    
    [<Test>] member x.
        ``when expression is x = x answer is true`` ()=
            EvaluateExpressionWithVariables m "x = x" |> should equal true
    
    [<Test>] member x.
        ``when expression is x = y answer is false`` ()=
            EvaluateExpressionWithVariables m "x = y" |> should equal false
            