module FCalculator.Main

open System
open System.Text.RegularExpressions

exception SyntaxError of string

type Node =
    | Value of decimal
    | QuotedString of string
    | Variable of string
    | Function of string * Node list
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
        
let rec Evaluate (f:Map<string, obj list -> obj>) (m:Map<string,Object>) node =
    match node with
    | Value n -> box n
    | QuotedString s -> upcast s
    | Variable s -> m.[s]
    | Function (name, nodes) -> (f.[name] [ for n in nodes -> Evaluate f m n ])
    | Add (l, r) -> box(todec(Evaluate f m l) + todec(Evaluate f m r))
    | Substract (l, r) -> box(todec(Evaluate f m l) - todec(Evaluate f m r))
    | Multiply (l, r) -> box(todec(Evaluate f m l) * todec(Evaluate f m r))
    | Divide (l, r) -> box(todec(Evaluate f m l) / todec(Evaluate f m r))
    | Modulo (l ,r) -> box(todec(Evaluate f m l) % todec(Evaluate f m r))
    | Equality (l, r) -> box(equals(Evaluate f m l) (Evaluate f m r))
    | Inequality (l, r) -> box(not(equals(Evaluate f m l) (Evaluate f m r)))
    | GreaterThan (l, r) -> box(todec(Evaluate f m l) > todec(Evaluate f m r))
    | LesserThan (l, r) -> box(todec(Evaluate f m l) < todec(Evaluate f m r))
    | GreaterOrEqualThan (l, r) -> box(todec(Evaluate f m l) >= todec(Evaluate f m r))
    | LesserOrEqualThan (l, r) -> box(todec(Evaluate f m l) <= todec(Evaluate f m r))
    | LogicalAnd (l, r) -> box(tobool(Evaluate f m l) && tobool(Evaluate f m r))
    | LogicalOr (l, r) -> box(tobool(Evaluate f m l) || tobool(Evaluate f m r))

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
              ||> ParseFunctions
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

and ParseFunctions (mappings:Map<string,Node>) (str:string) =
    let m = Regex("[a-z]\w*\s*\(", RegexOptions.Compiled ||| RegexOptions.IgnoreCase).Match(str)
   
    if m.Success then
        let beginFunc = m.Index
        let funcName = str.Substring(m.Index)
                       |> Seq.takeWhile (fun c ->
                            c <> ' ' && c <> '\t' && c <> '(')
                       |> Seq.map string
                       |> String.concat ""
        
        let escape = ref false
        let instring = ref false
        let pCount = ref 0
        
        let mutable curParam = ""
        let mutable funcParams = []
        let mutable parsed = false
        let mutable idx = str.IndexOf('(', m.Index + funcName.Length) + 1
        
        while not parsed do
           let c = str.[idx]
           idx <- idx + 1
           match c with
           | '"' -> if !instring then 
                        if !escape then 
                            escape := false
                        else
                            instring := false
                    else
                        instring := true
           | '\\' -> if !instring then
                        escape := true
                     else
                        raise (SyntaxError "Invalid escape")
           | '(' -> if not !instring then
                        pCount := !pCount + 1
           | ')' -> if not !instring then
                        pCount := !pCount - 1
                        parsed <- true
           | ',' -> if not !instring then
                        funcParams <- List.append funcParams [curParam]
                        curParam <- ""
           | _ -> ()
           if not parsed then curParam <- curParam + (string c)

        if curParam.Length > 0 then 
            funcParams <- List.append funcParams [curParam]

        let fp = funcParams
        let node = Function(funcName, [for p in fp -> ParseString mappings p])
        
        let id = "¤" + (string mappings.Count) + "¤"
        let nstr = str.[0..m.Index-1] + id + str.[idx..str.Length-1]
        let nmap = mappings.Add(id, node)
        
        (nmap, nstr)
    else
        (mappings, str)

and ParseVariables (mappings:Map<string,Node>) (str:string) =
    let m = Regex("([a-z]\w*)", RegexOptions.Compiled ||| RegexOptions.IgnoreCase).Match(str)
   
    if m.Success then
        HandleMatch m mappings str (fun mappings g -> Variable(g.[1].Value))
    else
        (mappings, str)
 

let EvaluateString str =
    ParseString Map.empty str |> Evaluate Map.empty Map.empty

let EvaluateStringWithVariables variables str =
    ParseString Map.empty str |> Evaluate Map.empty variables
    
let EvaluateStringWithFunctions funcs str =
    ParseString Map.empty str |> Evaluate funcs Map.empty