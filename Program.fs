type Expr =
    | Var of string
    | Int of int
    | Float of double
    | Sum of Expr * Expr
    | Mult of Expr * Expr
    | Say of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Print of Expr

let tokenize input =
    input.ToString().Split([| '('; ')'; ' '; '\n'; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let rec parseExpr tokens =
    match tokens with   
    | "say" :: var :: "=" :: rest ->
        // printfn "say %s = " var
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Say(var, expr1, expr2), tokens''
    | "sum" :: rest ->
        // printfn "sum"
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Sum(expr1, expr2), tokens''
    | "mult" :: rest ->
        // printfn "mult"
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Mult(expr1, expr2), tokens''
    | "if" :: rest ->
        // printfn "if"
        let cond, tokens' = parseExpr rest
        let expr1, tokens'' = parseExpr tokens'
        let expr2, tokens''' = parseExpr tokens''
        If(cond, expr1, expr2), tokens'''
    | "print" :: rest ->
        // printfn "print"
        let expr, tokens' = parseExpr rest
        Print(expr), tokens'
    | pattern :: rest ->
        let (success, value) = System.Int32.TryParse(pattern)

        if success then
            // printfn "int %s" pattern
            Int(value), rest
        else
            let (success, value) = System.Double.TryParse(pattern)
            if success then
                // printfn "float %s" pattern
                Float(value), rest
            else
                // printfn "str %s" pattern
                Var(pattern), rest
        
    | _ ->
        failwith "Invalid code"
let program = """
    say x = sum 4.5 5
    say y = mult 2 3
    print x
    print y
"""
let rec eval env expr =
    match expr with
    | Print e ->
        let value = eval env e
        printfn "%A" value
        value
    | Int n -> float n
    | Float f -> f
    | Sum(e1, e2) -> eval env e1 + eval env e2
    | Mult(e1, e2) -> eval env e1 * eval env e2
    | Var name -> Map.find name env
    | Say(name, e1, e2) ->
        let value = eval env e1
        let newEnv = Map.add name value env
        eval newEnv e2

let rec parseManyExprs tokens =
    if List.isEmpty tokens then
        [], []
    else 
        let expr, remainingTokens = parseExpr tokens
        let exprs, finalTokens = parseManyExprs remainingTokens
        expr::exprs, finalTokens

let parse input = 
    let tokens = tokenize input
    parseManyExprs tokens


let rec evalSeq env exprs =  
    match exprs with
    | [] -> ()
    | Print e :: rest ->
        let value = eval env e
        printfn "%A" value
        evalSeq env rest
    | Say(name, e1, e2) :: rest ->
        let value = eval env e1
        let newEnv = Map.add name value env
        evalSeq newEnv (e2 :: rest)
    | _ :: rest ->
        evalSeq env rest

let main =
    let exprs, _ = parse program
    evalSeq Map.empty exprs

main