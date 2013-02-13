module FCalculator.Main

open System
open System.Text.RegularExpressions

exception SyntaxError of string

type Node =
    | Value of decimal
    | QuotedString of string
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

let equals (o1:Object) (o2:Object) =
    match o1 with
    | :? bool -> Convert.ToBoolean(o1) = Convert.ToBoolean(o2)
    | :? decimal | :? int | :? float -> Convert.ToDecimal(o1) = Convert.ToDecimal(o2)
    | :? string -> o1.ToString() = o2.ToString()
    | _ -> o1 = o2
        
let rec Evaluate (m:Map<string,Object>) node =
    match node with
    | Value n -> box n
    | QuotedString s -> upcast s
    | Variable s -> m.[s]
    | Add (l, r) -> box(todec(Evaluate m l) + todec(Evaluate m r))
    | Substract (l, r) -> box(todec(Evaluate m l) - todec(Evaluate m r))
    | Multiply (l, r) -> box(todec(Evaluate m l) * todec(Evaluate m r))
    | Divide (l, r) -> box(todec(Evaluate m l) / todec(Evaluate m r))
    | Modulo (l ,r) -> box(todec(Evaluate m l) % todec(Evaluate m r))
    | Equality (l, r) -> box(equals(Evaluate m l) (Evaluate m r))
    | Inequality (l, r) -> box(not(equals(Evaluate m l) (Evaluate m r)))
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
    | "&&" -> LogicalAnd(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | "||" -> LogicalOr(
                ExtractOrCreateNode mappings groups.[1].Value,
                ExtractOrCreateNode mappings groups.[3].Value)
    | _ -> raise (SyntaxError "Invalid operator")
 
let HandleMatch (m:Match) (mappings:Map<string,Node>) (str:string) nodeCreator =
    let id = "¤" + (string mappings.Count) + "¤"
    let nstr = str.[0..m.Index-1] + id + str.[m.Index+m.Length..str.Length-1]
    let node = nodeCreator mappings m.Groups
    (mappings.Add(id, node), nstr)
 
let rec ParseOperators opers (mappings:Map<string,Node>) str =
    let rx = "((?:\d+(?:\.\d+)?)|¤\d+¤)\s*(" +
             (opers 
             |> List.map (fun c -> Regex.Escape c)
             |> String.concat "|") +
             ")\s*((?:\d+(?:\.\d+)?)|¤\d+¤)"
    
    let m = Regex(rx).Match(str)
    
    if m.Success then
        HandleMatch 
            m 
            mappings 
            str 
            (fun mappings g -> ParseGroups mappings g)
        ||> ParseOperators opers
    else
        (mappings, str)
        
let TokenizeReservedWords (mappings:Map<string,Node>) (str:string) =
    let nstr = str
                .Replace(" and ", " && ")
                .Replace(" or ", " || ")
    (mappings, nstr)
    
let rec ParseQuotedStrings (mappings:Map<string,Node>) (str:string) =
    let openIdx = str.IndexOf("\"")
    
    if openIdx >= 0 then
        let escaped = ref false
        
        let s = str.Substring(openIdx + 1) 
                |> Seq.takeWhile (fun c -> 
                    if c = '\\' then 
                        escaped := true
                        true
                    elif c = '"' then
                        if !escaped then
                            escaped := false
                            true
                        else
                            false
                    elif !escaped then
                        raise (SyntaxError "Invalid quoted string escape!")
                    else
                        true)
                |> Seq.map string
                |> String.concat ""
        
        let node = QuotedString s
        
        let id = "¤" + (string mappings.Count) + "¤"
        let nstr = str.[0..openIdx-1] + id + str.[openIdx+s.Length+2..str.Length-1]
        let nmap = mappings.Add(id, node)
        
        ParseQuotedStrings nmap nstr
    else
        (mappings, str)

let rec ParseString (mappings:Map<string,Node>) str =
    let res = ParseQuotedStrings mappings str
              ||> TokenizeReservedWords
              ||> ParseVariables
              ||> ParseParentheses
              ||> ParseOperators ["*";"/";"%"]
              ||> ParseOperators ["+";"-"]
              ||> ParseOperators ["<";">";"<=";">="]
              ||> ParseOperators ["=";"<>"]
              ||> ParseOperators ["&&";"||"]
    
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
        
        let id = "¤" + (string mappings.Count) + "¤"
        let nstr = str.[0..openIdx-1] + id + str.[openIdx+s.Length+2..str.Length-1]
        let nmap = mappings.Add(id, node)
        
        ParseParentheses nmap nstr
    else
        (mappings, str)
        
and ParseVariables (mappings:Map<string,Node>) (str:string) =
    let m = Regex("([a-z]\w*)", RegexOptions.Compiled ||| RegexOptions.IgnoreCase).Match(str)
   
    if m.Success then
        HandleMatch m mappings str (fun mappings g -> Variable(g.[1].Value))
    else
        (mappings, str)
 

let EvaluateString str =
    let top = ParseString Map.empty str
    
    Evaluate Map.empty top

let EvaluateStringWithVariables variables str =
    ParseString Map.empty str |> Evaluate variables