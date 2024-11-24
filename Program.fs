type Expr =
    | Var of string
    | Int of int
    | Float of double
    | Sum of Expr * Expr
    | Mult of Expr * Expr
    | Equal of Expr * Expr
    | Say of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Print of Expr
    | FuncDef of string * string * Expr
    | FuncCall of string * Expr

type Value = | Num of float | Fun of string * Expr

let tokenize input =
    input.ToString().Split([| '('; ')'; ' '; '\n'; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let rec parseExpr tokens =
    match tokens with
    | "Int" :: "-" :: valueStr :: rest ->
        let (success, value) = System.Int32.TryParse(valueStr)
        if success then
            Int(-value), rest
        else
            failwith "Invalid integer"
    | "Int" :: valueStr :: rest ->
        let (success, value) = System.Int32.TryParse(valueStr)
        if success then
            Int(value), rest
        else
            failwith "Invalid integer"
    | "equal" :: rest ->
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Equal(expr1, expr2), tokens''
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
    | "func" :: name :: param :: "=" :: rest ->
        let body, tokens' = parseExpr rest
        FuncDef(name, param, body), tokens'
    | "call" :: name :: rest ->
        let arg, tokens' = parseExpr rest
        FuncCall(name, arg), tokens'
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
    | Equal(e1, e2) ->
        let val1 = eval env e1
        let val2 = eval env e2
        if val1 = val2 then 1.0 else 0.0
    | Var name -> 
        match Map.tryFind name env with
        | Some(Num n) -> n
        | Some(Fun(_, _)) -> failwith "Function cannot be used as a value"
        | None -> failwithf "Variable '%s' not found in environment" name
    | Say(name, e1, e2) ->
        let value = eval env e1
        let newEnv = Map.add name (Num value) env
        eval newEnv e2
    | If(cond, e1, e2) ->
        if eval env cond <> 0.0 then
            eval env e1
        else
            eval env e2
    | FuncDef(name, param, body) ->
        let funcValue = Fun(param, body)
        let newEnv = Map.add name funcValue env
        0.0
    | FuncCall(name, arg) ->
        match Map.tryFind name env with
        | Some(Fun(param, body)) ->
            let argValue = eval env arg
            let newEnv = Map.add param (Num argValue) env
            eval newEnv body
        | Some(Num _) -> failwith "Not a function"
        | None -> failwithf "Function '%s' not found in environment" name

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
        let newEnv = Map.add name (Num value) env
        evalSeq newEnv (e2 :: rest)
    | FuncDef(name, param, body) :: rest ->
        let funcValue = Fun(param, body)
        let newEnv = Map.add name funcValue env
        evalSeq newEnv rest
    | expr :: rest ->
        eval env expr
        evalSeq env rest

let program = """
    func factorial x = if equal x 0 then 1 else mult x (call factorial sum x Int -1)
    say result = call factorial 6
    print result
"""

let main =
    let exprs, _ = parse program
    evalSeq Map.empty exprs

main