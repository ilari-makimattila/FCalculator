module FCalculator.Main

open System
open System.Text.RegularExpressions

type Node =
    | Value of float
    | Add of Node * Node
    | Substract of Node * Node
    | Multiply of Node * Node
    | Divide of Node * Node

let rec Evaluate (m:Map<string,Node>) node =
    match node with
    | Value n -> n
    | Add (l, r) -> Evaluate m l + Evaluate m r
    | Substract (l, r) -> Evaluate m l - Evaluate m r
    | Multiply (l, r) -> Evaluate m l * Evaluate m r
    | Divide (l, r) -> Evaluate m l / Evaluate m r

let ExtractOrCreateNode (mappings:Map<string,Node>) str =
    if mappings.ContainsKey str then
        mappings.[str]
    else
        Value (System.Double.Parse str)

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
    | _ -> Value 0.0
    
let rec ParseHighRankingOperators (mappings:Map<string,Node>) str =
    let m = Regex("((?:\d+(?:\.\d+)?)|n\d+n)\s*(\*|/)\s*((?:\d+(?:\.\d+)?)|n\d+n)").Match(str)
    
    if m.Success then
        let id = "n" + (string mappings.Count) + "n"
    
        let nstr = str.[0..m.Index-1] + id + str.[m.Index+m.Length..str.Length-1]
        let node = ParseGroups mappings m.Groups
        let nmap = mappings.Add(id, node)
    
        ParseHighRankingOperators nmap nstr
    else
        (mappings, str)

let rec ParseLowRankingOperators (mappings:Map<string,Node>) str =
    let m = Regex("((?:\d+(?:\.\d+)?)|n\d+n)\s*(\+|\-)\s*((?:\d+(?:\.\d+)?)|n\d+n)").Match(str)

    if m.Success then
        let id = "n" + (string mappings.Count) + "n"
    
        let nstr = str.[0..m.Index-1] + id + str.[m.Index+m.Length..str.Length-1]
        let node = ParseGroups mappings m.Groups
        let nmap = mappings.Add(id, node)
    
        ParseLowRankingOperators nmap nstr
    else
        (mappings, str)

let rec ParseString (mappings:Map<string,Node>) str =
    let parens = ParseParentheses mappings str
    let high = ParseHighRankingOperators (fst parens) (snd parens)
    let low = ParseLowRankingOperators (fst high) (snd high)
    (fst low).[(snd low)]
    
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
                    pCount.contents >= 0 || not (c = ')'))
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
