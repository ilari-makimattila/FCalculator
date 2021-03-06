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
let private ParseInfixGroups (mappings:Map<string,Node>) (groups:GroupCollection) =
    let node1 = ExtractOrCreateNode mappings groups.[1].Value
    let node2 = ExtractOrCreateNode mappings groups.[3].Value
    match groups.[2].Value with
    | "*" -> Multiply(node1, node2)
    | "/" -> Divide(node1, node2)
    | "+" -> Add(node1, node2)
    | "-" -> Substract(node1, node2)
    | "%" -> Modulo(node1, node2)
    | "=" -> Equality(node1, node2)
    | "<>" -> Inequality(node1, node2)
    | "<" -> LesserThan(node1, node2)
    | ">" -> GreaterThan(node1, node2)
    | "<=" -> LesserOrEqualThan(node1, node2)
    | ">=" -> GreaterOrEqualThan(node1, node2)
    | "&&" -> LogicalAnd(node1, node2)
    | "||" -> LogicalOr(node1, node2)
    | _ -> raise (SyntaxError ("Invalid infix operator " + groups.[2].Value))

let private ParseUnaryGroups (mappings:Map<string,Node>) (groups:GroupCollection) =
    let node = ExtractOrCreateNode mappings groups.[2].Value
    match groups.[1].Value with
    | "!" -> Not(node)
    | "-" -> Negation(node)
    | _ -> raise (SyntaxError ("Invalid prefix operator " + groups.[1].Value))

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
            (ParseInfixGroups mappings m.Groups)
        ||> ParseOperators opers
    else
        (mappings, str)

/// Parses the given prefix unary operators from the formula
let rec private ParseUnaryOperators opers mappings str =
    let rx = "(?:^|(?:[+\-*/%&|=<>!])\s*)(" +
             (opers 
             |> List.map (fun c -> Regex.Escape c)
             |> String.concat "|") +
             ")\s*((?:\d+(?:\.\d+)?)|¤\d+¤)"
    
    let m = Regex(rx).Match(str)
    
    if m.Success then
        HandleMatch 
            (m.Groups.[1].Index-1) 
            (m.Index+m.Length) 
            mappings
            str
            (ParseUnaryGroups mappings m.Groups)
        ||> ParseUnaryOperators opers
    else
        (mappings, str)

/// Converts reserved words to another forms
let private TokenizeReservedWords mappings (str:string) =
    let nstr = str
                .Replace(" and ", " && ")
                .Replace(" or ", " || ")
                .Replace("not", "!")
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

/// Parses datetime values like #2013-03-15 12:34:56#
let rec private ParseDateTimes mappings str =
    let m = Regex("#(.*?)#", RegexOptions.Compiled).Match(str)
    
    if m.Success then
        HandleMatch (m.Index-1) (m.Index+m.Length) mappings str (Date m.Groups.[1].Value)
        ||> ParseDateTimes
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

/// Parses variable definitions
let rec private ParseVariables mappings str =
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
        ||> ParseVariables
    else
        (mappings, str)

/// Parses the given string to an expression tree
let rec internal ParseString mappings str =
    let res = ParseQuotedStrings mappings str
              ||> ParseDateTimes
              ||> TokenizeReservedWords
              ||> ParseFunctions
              ||> ParseVariables
              ||> ParseParentheses
              ||> ParseUnaryOperators ["!";"-"]
              ||> ParseOperators ["*";"/";"%"]
              ||> ParseOperators ["+";"-"]
              ||> ParseOperators ["<";">";"<=";">="]
              ||> ParseOperators ["=";"<>"]
              ||> ParseOperators ["&&"]
              ||> ParseOperators ["||"]
              ||> ParseValues
    
    let d = fst res
    let k = snd res
    
    if d.ContainsKey k then
        d.[k]
    else
        raise (SyntaxError ("Invalid token " + k))

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
        
        let pCount = ref 0
        
        let mutable curParam = ""
        let mutable funcParams = []
        let mutable parsed = false
        let mutable skip = false
        let mutable idx = str.IndexOf('(', m.Index + funcName.Length) + 1
        
        while not parsed && idx < str.Length do
           let c = str.[idx]
           idx <- idx + 1
           match c with
           | '(' -> pCount := !pCount + 1
           | ')' -> pCount := !pCount - 1
                    parsed <- !pCount = -1
           | ',' -> funcParams <- List.append funcParams [curParam.Trim([|' '; '\t'|])]
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


/// Parses the given expression to an expression tree. The returned node can be
/// passed to <see cref="FCalculator.Evaluator.Evaluate"/>. By using this you
/// can cache the parsed expression.
let ParseExpression str =
    ParseString Map.empty str