module FCalculator.Main

open System
open System.Text.RegularExpressions

type Node =
    | Value of float
    | Add of Node * Node
    | Substract of Node * Node
    | Multiply of Node * Node
    | Divide of Node * Node

let rec Evaluate node =
    match node with
    | Value n -> n
    | Add (l, r) -> Evaluate l + Evaluate r
    | Substract (l, r) -> Evaluate l - Evaluate r
    | Multiply (l, r) -> Evaluate l * Evaluate r
    | Divide (l, r) -> Evaluate l / Evaluate r

let ExtractOrCreateNode (mappings:Map<string,Node>) str =
    if mappings.ContainsKey str then
        mappings.[str]
    else
        Value(System.Double.Parse(str))

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
    | _ -> Value(0.0) 
    
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

let EvaluateString str =
    let high = ParseHighRankingOperators Map.empty str
    let low = ParseLowRankingOperators (fst high) (snd high)
    
    let top = (fst low).[snd low]
    
    Evaluate top
