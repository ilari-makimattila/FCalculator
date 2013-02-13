module FCalculator.Main

open System
open System.Text.RegularExpressions

type Node =
    | Value of decimal
    | Variable of string
    | Add of Node * Node
    | Substract of Node * Node
    | Multiply of Node * Node
    | Divide of Node * Node
    | Modulo of Node * Node
    | Equality of Node * Node
    | Inequality of Node * Node
    | GreaterThan of Node * Node
    | LesserThan of Node * Node
    | GreaterOrEqualThan of Node * Node
    | LesserOrEqualThan of Node * Node
    | LogicalAnd of Node * Node
    | LogicalOr of Node * Node

let todec (obj:Object) = Convert.ToDecimal obj
let tobool (obj:Object) = Convert.ToBoolean obj

let rec Evaluate (m:Map<string,Object>) node =
    match node with
    | Value n -> box n
    | Variable s -> m.[s]
    | Add (l, r) -> box(todec(Evaluate m l) + todec(Evaluate m r))
    | Substract (l, r) -> box(todec(Evaluate m l) - todec(Evaluate m r))
    | Multiply (l, r) -> box(todec(Evaluate m l) * todec(Evaluate m r))
    | Divide (l, r) -> box(todec(Evaluate m l) / todec(Evaluate m r))
    | Modulo (l ,r) -> box(todec(Evaluate m l) % todec(Evaluate m r))
    | Equality (l, r) -> box(Evaluate m l = Evaluate m r)
    | Inequality (l, r) -> box(Evaluate m l <> Evaluate m r)
    | GreaterThan (l, r) -> box(todec(Evaluate m l) > todec(Evaluate m r))
    | LesserThan (l, r) -> box(todec(Evaluate m l) < todec(Evaluate m r))
    | GreaterOrEqualThan (l, r) -> box(todec(Evaluate m l) >= todec(Evaluate m r))
    | LesserOrEqualThan (l, r) -> box(todec(Evaluate m l) <= todec(Evaluate m r))
    | LogicalAnd (l, r) -> box(tobool(Evaluate m l) && tobool(Evaluate m r))
    | LogicalOr (l, r) -> box(tobool(Evaluate m l) || tobool(Evaluate m r))


let ExtractOrCreateNode (mappings:Map<string,Node>) str =
    if mappings.ContainsKey str then
        mappings.[str]
    else
        Value (System.Decimal.Parse str)

let ParseGroups (mappings:Map<string,Node>) (groups:GroupCollection) =
    match groups.[2].Value with
    | "*" -> Multiply(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "/" -> Divide(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "+" -> Add(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "-" -> Substract(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "%" -> Modulo(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "=" -> Equality(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "<>" -> Inequality(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "<" -> LesserThan(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | ">" -> GreaterThan(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "<=" -> LesserOrEqualThan(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | ">=" -> GreaterOrEqualThan(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "and" -> LogicalAnd(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "or" -> LogicalOr(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | _ -> raise (Exception "Invalid operator")
 
let rec ParseOperators opers (mappings:Map<string,Node>) str =
    let rx = "((?:\d+(?:\.\d+)?)|n\d+n)\s*(" +
             (opers 
             |> List.map (fun c -> Regex.Escape c)
             |> String.concat "|") +
             ")\s*((?:\d+(?:\.\d+)?)|n\d+n)"
    
    let m = Regex(rx).Match(str)
    
    if m.Success then
        let id = "n" + (string mappings.Count) + "n"
    
        let nstr = str.[0..m.Index-1] + id + str.[m.Index+m.Length..str.Length-1]
        let node = ParseGroups mappings m.Groups
        let nmap = mappings.Add(id, node)
    
        ParseOperators opers nmap nstr
    else
        (mappings, str)

let rec ParseString (mappings:Map<string,Node>) str =
    let res = ParseParentheses mappings str
              ||> ParseOperators ["*";"/";"%"]
              ||> ParseOperators ["+";"-"]
              ||> ParseOperators ["<";">";"<=";">="]
              ||> ParseOperators ["=";"<>"]
              ||> ParseOperators ["and";"or"]
    
    (fst res).[(snd res)]
    
and ParseParentheses (mappings:Map<string,Node>) (str:string) =
    let openIdx = str.IndexOf('(')
    
    if openIdx >= 0 then
    
        let pCount = ref 0
        
        let s = str.Substring(openIdx + 1) 
                |> Seq.takeWhile (fun c -> 
                    if c = '(' then 
                        pCount := pCount.contents + 1
                    elif c = ')' then
                        pCount := pCount.contents - 1
                    pCount.contents >= 0 || c <> ')')
                |> Seq.map string
                |> String.concat ""
        
        let node = ParseString mappings s
        
        let id = "n" + (string mappings.Count) + "n"
        let nstr = str.[0..openIdx-1] + id + str.[openIdx+s.Length+2..str.Length-1]
        let nmap = mappings.Add(id, node)
        
        ParseParentheses nmap nstr
    else
        (mappings, str)
 

let EvaluateString str =
    let top = ParseString Map.empty str
    
    Evaluate Map.empty top
