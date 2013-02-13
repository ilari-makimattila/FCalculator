module FCalculator.Tests.Relation

open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateString is given a valid relation expression`` ()=

    [<Test>] member x.
        ``when expression is 1 <= 1 answer is true`` ()=
            EvaluateString "1 <= 1" |> should equal true
    
    [<Test>] member x.
        ``when expression is 1 >= 1 answer is true`` ()=
            EvaluateString "1 >= 1" |> should equal true
     
    [<Test>] member x.
        ``when expression is 1 <= 2 answer is true`` ()=
            EvaluateString "1 <= 2" |> should equal true
            
    [<Test>] member x.
        ``when expression is 2 >= 1 answer is true`` ()=
            EvaluateString "2 >= 1" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 >= 2 answer is false`` ()=
            EvaluateString "1 >= 2" |> should equal false 
            
    [<Test>] member x.
        ``when expression is 2 <= 1 answer is false`` ()=
            EvaluateString "2 <= 1" |> should equal false
            
    
    
    
    [<Test>] member x.
        ``when expression is 1 < 1 answer is false`` ()=
            EvaluateString "1 < 1" |> should equal false
    
    [<Test>] member x.
        ``when expression is 1 > 1 answer is false`` ()=
            EvaluateString "1 > 1" |> should equal false
     
    [<Test>] member x.
        ``when expression is 1 < 2 answer is true`` ()=
            EvaluateString "1 < 2" |> should equal true
            
    [<Test>] member x.
        ``when expression is 2 > 1 answer is true`` ()=
            EvaluateString "2 > 1" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 > 2 answer is false`` ()=
            EvaluateString "1 > 2" |> should equal false 
            
    [<Test>] member x.
        ``when expression is 2 < 1 answer is false`` ()=
            EvaluateString "2 < 1" |> should equal false
            
   