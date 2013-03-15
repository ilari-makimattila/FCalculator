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

// construct builtin functions
let private builtins = 
    let l = [
        ("if", fun (args:obj list) -> if tobool(args.[0]) then args.[1] else args.[2])
        ("in", fun (args:obj list) -> box(List.exists (fun i -> i = args.Head) args.Tail))
    ]
    new Map<string, obj list -> obj>(l)

// construct builtin variables
let private constants =
    let l = [
        ("true", box true)
        ("false", box false)
    ]
    new Map<string, obj>(l)

let rec private Eval (f:Map<string, obj list -> obj>) (v:Map<string,obj>) node =
    match node with
    | Value n -> box n
    | QuotedString s -> upcast s
    | Variable s -> v.[s]
    | Date s -> box(System.DateTime.Parse s)
    | Function (name, nodes) -> (f.[name] [ for n in nodes -> Eval f v n ])
    | Add (l, r) -> box(todec(Eval f v l) + todec(Eval f v r))
    | Substract (l, r) -> box(todec(Eval f v l) - todec(Eval f v r))
    | Multiply (l, r) -> box(todec(Eval f v l) * todec(Eval f v r))
    | Divide (l, r) -> box(todec(Eval f v l) / todec(Eval f v r))
    | Modulo (l ,r) -> box(todec(Eval f v l) % todec(Eval f v r))
    | Equality (l, r) -> box(equals(Eval f v l) (Eval f v r))
    | Inequality (l, r) -> box(not(equals(Eval f v l) (Eval f v r)))
    | GreaterThan (l, r) -> box(todec(Eval f v l) > todec(Eval f v r))
    | LesserThan (l, r) -> box(todec(Eval f v l) < todec(Eval f v r))
    | GreaterOrEqualThan (l, r) -> box(todec(Eval f v l) >= todec(Eval f v r))
    | LesserOrEqualThan (l, r) -> box(todec(Eval f v l) <= todec(Eval f v r))
    | LogicalAnd (l, r) -> box(tobool(Eval f v l) && tobool(Eval f v r))
    | LogicalOr (l, r) -> box(tobool(Eval f v l) || tobool(Eval f v r))
    | Not n -> box(not(unbox(Eval f v n)))
    | Negation n -> box(-(todec(Eval f v n)))

/// <summary>Evaluates the given node with given functions and variables</summary>
/// <param name="functions">A map of functions</param>
/// <param name="variables">A map of variables</param>
/// <returns>
/// An object which can be bool, decimal or string, depending on the formula
/// </returns>
/// <remarks>
/// Function and variable names are case sensitive.
/// </remarks>
let Evaluate (functions:Map<string, obj list -> obj>) (variables:Map<string,obj>) node =
    let mutable funcs = builtins
    for t in functions do
        funcs <- funcs.Add(t.Key, functions.[t.Key])                  
    
    let mutable vars = constants
    for t in variables do
        vars <- vars.Add(t.Key, variables.[t.Key])
    
    Eval funcs vars node

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
