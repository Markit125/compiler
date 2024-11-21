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
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Say(var, expr1, expr2), tokens''
    | "sum" :: rest ->
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Sum(expr1, expr2), tokens''
    | "mult" :: rest ->
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Mult(expr1, expr2), tokens''
    | "if" :: rest ->
        let cond, tokens' = parseExpr rest
        match tokens' with
        | "then" :: thenTokens ->
            let expr1, tokens'' = parseExpr thenTokens
            match tokens'' with
            | "else" :: elseTokens ->
                let expr2, tokens''' = parseExpr elseTokens
                If(cond, expr1, expr2), tokens'''
            | _ -> failwith "Expected 'else'"
        | _ -> failwith "Expected 'then'"
    | "print" :: rest ->
        let expr, tokens' = parseExpr rest
        Print(expr), tokens'
    | pattern :: rest ->
        let (success, value) = System.Int32.TryParse(pattern)
        if success then
            Int(value), rest
        else
            let (success, value) = System.Double.TryParse(pattern)
            if success then
                Float(value), rest
            else
                Var(pattern), rest
    | _ ->
        failwith "Invalid code"

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
    | If(cond, e1, e2) ->
        if eval env cond <> 0.0 then
            eval env e1
        else
            eval env e2

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
    | expr :: rest ->
        eval env expr
        evalSeq env rest

let program = """
    say x = sum 4.5 5
    say y = mult 2 3
    say z = if sum x y then 10 else 20
    print z
    say w = if mult 0 x then 10 else 20
    print w

    say a = 1
    say b = 1
    say minusB = mult b -1
    say equals = if sum a minusB then 0 else 1
    print equals
"""

let main =
    let exprs, _ = parse program
    evalSeq Map.empty exprs

main