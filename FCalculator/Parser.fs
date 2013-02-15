module FCalculator.Parser

open System
open System.Text.RegularExpressions
open FCalculator.Main

/// Thrown when the formula can not be parsed
exception SyntaxError of string

/// Helper for using an existing node or creating a new one
let private ExtractOrCreateNode (mappings:Map<string,Node>) str =
    if mappings.ContainsKey str then
        mappings.[str]
    else
        Value (Decimal.Parse str)

/// Creates a node from regex match groups
let private ParseGroups (mappings:Map<string,Node>) (groups:GroupCollection) =
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
 
/// Helper for handling a regex match
let private HandleMatch start stop (mappings:Map<string,Node>) (str:string) node =
    let id = "¤" + (string mappings.Count) + "¤"
    let nstr = str.[0..start] + id + str.[stop..str.Length-1]
    (mappings.Add(id, node), nstr)
 
/// Parses the given infix operators from the formula
let rec private ParseOperators opers mappings str =
    let rx = "((?:\d+(?:\.\d+)?)|¤\d+¤)\s*(" +
             (opers 
             |> List.map (fun c -> Regex.Escape c)
             |> String.concat "|") +
             ")\s*((?:\d+(?:\.\d+)?)|¤\d+¤)"
    
    let m = Regex(rx).Match(str)
    
    if m.Success then
        HandleMatch 
            (m.Index-1) 
            (m.Index+m.Length) 
            mappings
            str
            (ParseGroups mappings m.Groups)
        ||> ParseOperators opers
    else
        (mappings, str)

/// Converts reserved words to another forms
let private TokenizeReservedWords mappings (str:string) =
    let nstr = str
                .Replace(" and ", " && ")
                .Replace(" or ", " || ")
    (mappings, nstr)

/// Parses strings between double quotes
let rec private ParseQuotedStrings (mappings:Map<string,Node>) (str:string) =
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
        
        HandleMatch (openIdx-1) (openIdx+s.Length+2) mappings str node
        ||> ParseQuotedStrings
    else
        (mappings, str)

/// Parses constant values, such as function parameters, from the formula
let private ParseValues mappings str =
    let m = Regex(
                "(?:^\d+(?:\.\d+)?$)|(\s\d+(?:\.\d+)?\s)", 
                RegexOptions.Compiled ||| RegexOptions.IgnoreCase).Match(str)
    
    if m.Success then
        HandleMatch
            (m.Index-1)
            (m.Index+m.Length)
            mappings
            str
            (ExtractOrCreateNode mappings m.Groups.[0].Value)
    else
        (mappings, str)

/// Parses the given string to an expression tree
let rec internal ParseString mappings str =
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
              ||> ParseValues
    
    (fst res).[(snd res)]

/// Parses the strings between parentheses  
and private ParseParentheses mappings (str:string) =
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

        HandleMatch (openIdx-1) (openIdx+s.Length+2) mappings str node
        ||> ParseParentheses
    else
        (mappings, str)

/// Parses function definitions from the string
and private ParseFunctions mappings str =
    let m = Regex(
                "[a-z]\w*\s*\(",
                RegexOptions.Compiled ||| RegexOptions.IgnoreCase).Match(str)
   
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
        let mutable skip = false
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
                        funcParams <- List.append funcParams [curParam.Trim([|' '; '\t'|])]
                        curParam <- ""
                        skip <- true
           | _ -> ()
           if not parsed && not skip then curParam <- curParam + (string c)
           skip <- false

        if curParam.Length > 0 then 
            funcParams <- List.append funcParams [curParam.Trim([|' '; '\t'|])]

        let fp = funcParams
        let node = Function(funcName, [for p in fp -> ParseString mappings p])
        
        HandleMatch (m.Index-1) idx mappings str node 
        ||> ParseFunctions
    else
        (mappings, str)

/// Parses variable definitions
and private ParseVariables mappings str =
    let m = Regex(
                "([a-z]\w*)", 
                RegexOptions.Compiled ||| RegexOptions.IgnoreCase).Match(str)
   
    if m.Success then
        HandleMatch 
            (m.Index-1) 
            (m.Index+m.Length) 
            mappings 
            str 
            (Variable(m.Groups.[1].Value))
    else
        (mappings, str)

/// Parses the given expression to an expression tree. The returned node can be
/// passed to <see cref="FCalculator.Evaluator.Evaluate"/>. By using this you
/// can cache the parsed expression.
let ParseExpression str =
    ParseString Map.empty str