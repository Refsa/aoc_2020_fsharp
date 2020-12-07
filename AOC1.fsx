module AOC1

#r "./bin/debug/netcoreapp3.1/FParsec.dll"
#r "./bin/debug/netcoreapp3.1/FParsecCS.dll"

open FParsec

// let SParser: Parser<string, 'u> -> Parser<string, 'u>

let matches2_2020 (a, b) = if a + b = 2020 then a * b else 0

let matches3_2020 (a, b, c) =
    if a + b + c = 2020 then a * b * c else 0

let str_nl s = pstring s .>> spaces
let int_nl = pstring "" >>. pint32 .>> spaces

let check_num fn num nums =
    nums
    |> List.map (fun b -> fn (num, b))
    |> List.filter (fun a -> a <> 0)
    |> List.tryHead

let check_num_2 fn num1 num2 nums =
    nums
    |> List.map (fun b -> fn (num1, num2, b))
    |> List.filter (fun a -> a <> 0)
    |> List.tryHead

let loop nums fn =
        nums
        |> List.mapi (fun i a -> fn a (nums |> List.skip (i)))

let loop2 nums fn =
    nums
    |> List.mapi (fun i a -> loop (nums |> List.skip (i)) (fn a))
    |> List.concat

let handle_num_list nums =
    let result =
        loop nums (check_num matches2_2020)
        |> List.filter (fun a -> a.IsSome)
        |> List.head
        |> fun a -> a.Value

    result

let handle_num_list_2 nums =
    let result =
        loop2 nums (check_num_2 matches3_2020)
        |> List.filter (fun a -> a.IsSome)
        |> List.head
        |> fun a -> a.Value

    result

let run_part1 input =
    let result1 =
        run (many int_nl |>> handle_num_list) input

    let result =
        match result1 with
        | Success (result, _, _) -> result.ToString()
        | Failure (error, _, _) -> sprintf "Error: %s" error

    result

let run_part2 input =
    let result1 =
        run (many int_nl |>> handle_num_list_2) input

    let result =
        match result1 with
        | Success (result, _, _) -> result.ToString()
        | Failure (error, _, _) -> sprintf "Error: %s" error

    result

let run input =
    sprintf "Part1: %s\nPart2: %s" (run_part1 input) (run_part2 input)