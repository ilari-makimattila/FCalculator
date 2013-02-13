module FCalculator.Tests.Constants

open NUnit.Framework
open FsUnit
open FCalculator.Main

[<TestFixture>]
type ``Given EvaluateString parameter has constants`` ()=

    [<Test>] member x.
        ``when expression is "foo" = "foo" answer is true`` ()=
            EvaluateString "\"foo\" = \"foo\"" |> should equal true
            
    [<Test>] member x.
        ``when expression is "fo\"o" = "foo" answer is false`` ()=
            EvaluateString "\"fo\\\"o\" = \"foo\"" |> should equal false

    [<Test>] member x.
        ``when expression is "fo\"o" = "fo\"o" answer is true`` ()=
            EvaluateString "\"fo\\\"o\" = \"fo\\\"o\"" |> should equal true