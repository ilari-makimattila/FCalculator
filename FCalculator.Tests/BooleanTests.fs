module FCalculator.Tests.Boolean

open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateString is given a valid boolean expression`` ()=

    [<Test>] member x.
        ``when expression is 1 = 1 answer is true`` ()=
            EvaluateString "1 = 1" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 = 2 answer is false`` ()=
            EvaluateString "1 = 2" |> should equal false
            
    [<Test>] member x.
        ``when expression is 1 <> 2 answer is true`` ()=
            EvaluateString "1 <> 2" |> should equal true
            
    [<Test>] member x.
        ``when expression is 1 <> 1 answer is false`` ()=
            EvaluateString "1 <> 1" |> should equal false
            
    [<Test>] member x.
        ``when expression is 2 and 2 answer is true`` ()=
            EvaluateString "2 and 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 1 and 2 answer is true`` ()=
            EvaluateString "1 and 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 0 and 0 answer is false`` ()=
            EvaluateString "0 and 0" |> should equal false
            
    [<Test>] member x.
        ``when expression is 2 or 2 answer is true`` ()=
            EvaluateString "2 or 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 1 or 2 answer is true`` ()=
            EvaluateString "1 or 2" |> should equal true
    
    [<Test>] member x.
        ``when expression is 0 or 0 answer is false`` ()=
            EvaluateString "0 or 0" |> should equal false
            
    [<Test>] member x.
        ``when expression is 1 or 0 answer is true`` ()=
            EvaluateString "1 or 0" |> should equal true