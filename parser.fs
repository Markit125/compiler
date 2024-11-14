type expression =
    | Variable of string
    | Integer of int
    | Float of float
    | Sum of expression * expression
    | Say of string * expression * expression
    | Provided of expression * expression * expression


let rec parse tokens =
    match tokens with
    | "say" :: name :: "=" :: expr ->
        let fExpr, fTail = parse expr
        let sExpr, sTail = parse fTail
        Say(name, fExpr, sExpr), sTail
    | "sum" :: expr ->
        let fExpr, fTail = parse expr
        let sExpr, sTail = parse fTail
        Sum(fExpr, sExpr), sTail
    | floatNum :: expr when System.Float32.TryParse(floatnum).IsSuccess ->
        Float(float floatNum), expr
    | integerNum :: expr when System.Int32.TryParse(integerNum).IsSuccess ->
        Integer(int integerNum), expr
    | var :: expr ->
        Variable(var), expr

