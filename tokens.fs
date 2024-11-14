let tokens input =
    input.Split [| ' '; '\t'; '\n'; |]
    |> Array.ToList