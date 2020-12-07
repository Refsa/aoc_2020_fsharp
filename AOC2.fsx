module AOC2

#r "./bin/debug/netcoreapp3.1/FParsec.dll"
#r "./bin/debug/netcoreapp3.1/FParsecCS.dll"

open FParsec

let str_nl s = pstring s .>> spaces

let unwrap_result (parser: ParserResult<'a, 'b>): 'a =
    match parser with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> Unchecked.defaultof<'a>

let get_some_count (target: 'a option list) =
    target
    |> List.filter (fun a -> a.IsSome)
    |> List.length
    |> fun a -> a.ToString()

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

    let password_parser_2 (min: int, max: int) wanted password =
        password
        |> String.mapi (fun i a -> 
            match (i + 1) with
            | i when (i = min || i = max) && a = wanted -> 'X'
            | _ -> 'O'
        )
        |> String.filter (fun a -> a = 'X')
        |> String.length
        |> function
            | 1 -> Some(0)
            | _ -> None

    let parse1 =
        run (many (pipe3 parse_range parse_char parse_rest password_parser_1)) input |> unwrap_result |> get_some_count

    let parse2 =
        run (many (pipe3 parse_range parse_char parse_rest password_parser_2)) input |> unwrap_result |> get_some_count

    sprintf "Part1: %s\nPart2: %s" parse1 parse2