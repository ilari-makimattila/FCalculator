module FCalculator.Tests.Variables

open System
open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateStringWithVariables has a map of variables and a valid expression`` ()=    
    let m = new Map<string,Object>([ ("x", box 1) ])       
    
    [<Test>] member x.
        ``when expression is x = 1 answer is true`` ()=    
            EvaluateStringWithVariables m "x = 1" |> should equal true
           
            