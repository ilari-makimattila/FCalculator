module FCalculator.Tests.Constants

open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateString parameter has constants`` ()=

    [<Test>] member x.
        ``when expression is 2 answer is 2`` ()=
            EvaluateString "2" |> should equal 2.0m

    [<Test>] member x.
        ``when expression is 2.5 answer is 2.5`` ()=
            EvaluateString "2.5" |> should equal 2.5m

    [<Test>] member x.
        ``when expression is "foo" = "foo" answer is true`` ()=
            EvaluateString "\"foo\" = \"foo\"" |> should equal true
            
    [<Test>] member x.
        ``when expression is "fo\"o" = "foo" answer is false`` ()=
            EvaluateString "\"fo\\\"o\" = \"foo\"" |> should equal false

    [<Test>] member x.
        ``when expression is "fo\"o" = "fo\"o" answer is true`` ()=
            EvaluateString "\"fo\\\"o\" = \"fo\\\"o\"" |> should equal true