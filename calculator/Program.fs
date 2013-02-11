module calculator.Main

open System
open FCalculator.Main

[<EntryPoint>]
let main args = 
    let str = String.Join(" ", args)
    
    
    printfn "%A" (EvaluateString str)
    0

