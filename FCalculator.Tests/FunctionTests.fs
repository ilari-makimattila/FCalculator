module FCalculator.Tests.Functions

open System
open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateStringWithFunctions has a map of funcs and a valid expression`` ()=    
    let f = new Map<string, obj list -> obj>([ 
                                             ("square", 
                                              fun o -> box(Convert.ToDecimal(o.Head) * Convert.ToDecimal(o.Head))) ])

    [<Test>] member x.
        ``when expression is square(2) answer is 4`` ()=    
            EvaluateStringWithFunctions f "square(2)" |> should equal 4.0m
    
    [<Test>] member x.
        ``when expression is square(1 + 1) answer is 4`` ()=    
            EvaluateStringWithFunctions f "square(1 + 1)" |> should equal 4.0m
            