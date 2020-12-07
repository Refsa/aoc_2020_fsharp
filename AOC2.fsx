module AOC2

#r "./bin/debug/netcoreapp3.1/FParsec.dll"
#r "./bin/debug/netcoreapp3.1/FParsecCS.dll"

open FParsec

let str_nl s = pstring s .>> spaces

let unwrap_result (parser: ParserResult<'a, 'b>): 'a =
    match parser with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> Unchecked.defaultof<'a>

let run input =
    let parse_range =
        pipe2 (pstring "" >>. pint32) (skipChar '-' >>. pint32) (fun a b -> (a, b))

    let parse_char = spaces >>. anyChar .>> pstring ": "
    let parse_rest = restOfLine true

    let parse_password wanted = manyCharsTill (pchar wanted) eof

    let password_parser_1 (min, max) wanted password =
        password
        |> String.filter (fun a -> a = wanted)
        |> String.length
        |> function
            | x when x >= min && x <= max -> Some(x)
            | 0
            | _ -> None

    let password_parser_2 (min, max) wanted password =
        password
        |> String.filter (fun a -> a = wanted)
        |> String.length
        |> function
            | x when x >= min && x <= max -> Some(x)
            | 0
            | _ -> None

    let parse =
        run (many (pipe3 parse_range parse_char parse_rest password_parser_1)) input

    let result =
        match parse with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> []

    result
    |> List.filter (fun a -> a.IsSome)
    |> List.length
    |> fun a -> a.ToString()
