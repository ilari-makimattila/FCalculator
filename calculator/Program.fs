module calculator.Main

open System
open FCalculator.Evaluator

[<EntryPoint>]
let main args = 
    let str = String.Join(" ", args)
    
    printfn "%A" (EvaluateExpression str)
    0

