# A mathematical expression library written in F# language

The parser can parse about any mathematical expression and evaluate the result,
including static variables and custom functions.

The parser is hand written using regular expressions (yes, I solved two 
problems ;) and simple string parsing. I wanted to do this without lexers
and any stuff like that.

## Usage

### Simple expressions

```fsharp
open FCalculator.Evaluator

let result = EvaluateExpression "1 + 2 * 3"
```

### Variables and functions

```fsharp
open FCalculator.Evaluator

let myFuncs = [ ("multiply", fun (args:obj list) -> (unbox args.[0]) * (unbox args.[1])) ] |> Map.ofList
let myVars = [ ("x", box 4); ("y", box 2) ] |> Map.ofList

let result = EvaluateExpressionWithFunctionsAndVariables myFuncs myVars "multiply(x, y)"
```

Function and variable names are always case sensitive.

### Parsing without evaluating

```fsharp
open FCalculator.Parser

let topNode = ParseExpression "1 * (2 + 2) / 4" 
```

## Bugs

There is no error handling. You might get a SyntaxError exception but they're
thrown only because something needs to be returned in those cases.

## Why?

This was my first touch to functional programming and I thought that writing
a string parser and evaluating expression trees would be a fun exercise. 
And it was.