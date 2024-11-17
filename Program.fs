type Expr =
    | Var of string
    | Int of int
    | Sum of Expr * Expr
    | Let of string * Expr * Expr
    | If of Expr * Expr * Expr
    | Print of Expr

let tokenize input =
    input.ToString().Split([| '('; ')'; ' '; '\n'; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let rec parseExpr tokens =
    match tokens with   
    | "let" :: var :: "=" :: rest ->
        printfn "let %s = " var
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Let(var, expr1, expr2), tokens''
    | "sum" :: rest ->
        printfn "sum"
        let expr1, tokens' = parseExpr rest
        let expr2, tokens'' = parseExpr tokens'
        Sum(expr1, expr2), tokens''
    | "if" :: rest ->
        printfn "if"
        let cond, tokens' = parseExpr rest
        let expr1, tokens'' = parseExpr tokens'
        let expr2, tokens''' = parseExpr tokens''
        If(cond, expr1, expr2), tokens'''
    | "print" :: rest ->
        printfn "print"
        let expr, tokens' = parseExpr rest
        Print(expr), tokens'
    | pattern :: rest ->
        printfn "int/str %s" pattern
        let (success, value) = System.Int32.TryParse(pattern)

        if success then
            Int(value), rest
        else
            Var(pattern), rest
    | _ ->
        failwith "Invalid code"

let parse input = 
    let tokens = tokenize input
    fst (parseExpr tokens)

let rec eval env expr =
    match expr with
    | Print e ->
        let value = eval env e
        printfn "%d" value
        value

    | Int n -> n
    | Sum(e1, e2) -> eval env e1 + eval env e2
    | Var name -> Map.find name env
    | Let(name, e1, e2) ->
        let value = eval env e1
        let newEnv = Map.add name value env
        eval newEnv e2
let program = "let x = 2 print x"

let main =
    let expr = parse program
    let result = eval Map.empty expr
    printfn "Result: %d" result

main
