module FCalculator.Evaluator

open System
open FCalculator.Main
open FCalculator.Parser

let private todec (obj:obj) = Convert.ToDecimal obj
let private tobool (obj:obj) = Convert.ToBoolean obj

let private equals (o1:obj) (o2:obj) =
    match o1 with
    | :? bool -> Convert.ToBoolean(o1) = Convert.ToBoolean(o2)
    | :? decimal | :? int | :? float -> Convert.ToDecimal(o1) = Convert.ToDecimal(o2)
    | :? string -> o1.ToString() = o2.ToString()
    | _ -> o1 = o2

/// <summary>Evaluates the given node with given functions and variables</summary>
/// <param name="functions">A map of functions</param>
/// <param name="variables">A map of variables</param>
/// <returns>
/// An object which can be bool, decimal or string, depending on the formula
/// </returns>
/// <remarks>
/// Function and variable names are case sensitive.
/// </remarks>
let rec Evaluate (functions:Map<string, obj list -> obj>) (variables:Map<string,obj>) node =
    match node with
    | Value n -> box n
    | QuotedString s -> upcast s
    | Variable s -> variables.[s]
    | Function (name, nodes) -> (functions.[name] [ for n in nodes -> Evaluate functions variables n ])
    | Add (l, r) -> box(todec(Evaluate functions variables l) + todec(Evaluate functions variables r))
    | Substract (l, r) -> box(todec(Evaluate functions variables l) - todec(Evaluate functions variables r))
    | Multiply (l, r) -> box(todec(Evaluate functions variables l) * todec(Evaluate functions variables r))
    | Divide (l, r) -> box(todec(Evaluate functions variables l) / todec(Evaluate functions variables r))
    | Modulo (l ,r) -> box(todec(Evaluate functions variables l) % todec(Evaluate functions variables r))
    | Equality (l, r) -> box(equals(Evaluate functions variables l) (Evaluate functions variables r))
    | Inequality (l, r) -> box(not(equals(Evaluate functions variables l) (Evaluate functions variables r)))
    | GreaterThan (l, r) -> box(todec(Evaluate functions variables l) > todec(Evaluate functions variables r))
    | LesserThan (l, r) -> box(todec(Evaluate functions variables l) < todec(Evaluate functions variables r))
    | GreaterOrEqualThan (l, r) -> box(todec(Evaluate functions variables l) >= todec(Evaluate functions variables r))
    | LesserOrEqualThan (l, r) -> box(todec(Evaluate functions variables l) <= todec(Evaluate functions variables r))
    | LogicalAnd (l, r) -> box(tobool(Evaluate functions variables l) && tobool(Evaluate functions variables r))
    | LogicalOr (l, r) -> box(tobool(Evaluate functions variables l) || tobool(Evaluate functions variables r))

/// Parse and evaluate a string
let EvaluateExpression str =
    ParseString Map.empty str |> Evaluate Map.empty Map.empty

/// Parse and evaluate a string with custom variables
let EvaluateExpressionWithVariables variables str =
    ParseString Map.empty str |> Evaluate Map.empty variables

/// Parse and evaluate a string with custom functions
let EvaluateExpressionWithFunctions funcs str =
    ParseString Map.empty str |> Evaluate funcs Map.empty

/// Parse and evaluate a strin with custom functions and variables
let EvaluateExpressionWithFunctionsAndVariables functions variables str =
    ParseString Map.empty str |> Evaluate functions variables
